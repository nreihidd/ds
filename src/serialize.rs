use ::{Value, EvalResult, EvalErr, Builtin};
use onetoone;
use std::rc::Rc;
use std::cell::RefCell;

thread_local!(pub static BUILTIN_SERIALIZE_MAPPING: RefCell<onetoone::OneToOne<&'static str, Builtin>> = RefCell::new(onetoone::OneToOne::new()));

fn escape_string(s: &str) -> String {
    s.to_string().replace("\\", "\\\\").replace("\n", "\\n")
}
fn unescape_string(s: &str) -> String {
    use std::fmt::Write;
    let mut r = String::new();
    let mut escaping = false;
    for c in s.chars() {
        if escaping {
            match c {
                '\\' if escaping => r.write_char('\\').unwrap(),
                'n' if escaping => r.write_char('\n').unwrap(),
                _ => panic!(format!("Bad escape sequence: {}", c)),
            }
        } else if c == '\\' {
            escaping = true;
        } else {
            r.write_char(c).unwrap();
        }
    }
    r
}

fn split_two<'a>(s: &'a str, by: char) -> (&'a str, &'a str) {
    let mut v = s.split(by);
    (v.next().unwrap(), v.next().unwrap())
}

pub fn deserialize(s: &str) -> EvalResult {
    enum Either<T, S> {
        Left(T),
        Right(S),
    }
    enum NodeState {
        Cons(usize, usize),
        Compound(usize),
    }

    let mut lines: Vec<&str> = s.split('\n').collect();
    lines.pop();
    let lines = lines;
    BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
        let builtin_mapping = builtin_mapping.borrow();
        // Create all primitives/cells
        let mut values: Vec<Either<NodeState, Rc<Value>>> = vec![];
        for (line_number, line) in lines.iter().enumerate() {

            macro_rules! try_unwrapo {
                ($e:expr) => (
                    match $e {
                        Some(v) => v,
                        None => return Err(EvalErr::Detailed(tree!({format!("Deserialize Error {}:{} on input line {}", line!(), column!(), line_number)}))),
                    }
                );
            }
            macro_rules! try_unwrap {
                ($e:expr) => (
                    match $e {
                        Ok(v) => v,
                        Err(e) => return Err(EvalErr::Detailed(tree!({format!("Deserialize Error {}:{} on input line {}", line!(), column!(), line_number)} {format!("{:?}", e)}))),
                    }
                )
            }

            let (t, v) = { let mut split = line.splitn(2, ' '); (try_unwrapo!(split.next()), split.next()) };
            values.push(match t {
                "Cons" => {
                    let (left, right) = split_two(try_unwrapo!(v), ' ');
                    Either::Left(NodeState::Cons(try_unwrap!(left.parse()), try_unwrap!(right.parse())))
                },
                "Boolean" => {
                    Either::Right(Rc::new(Value::Boolean(try_unwrap!(try_unwrapo!(v).parse()))))
                },
                "Number" => {
                    Either::Right(Rc::new(Value::Number(try_unwrap!(try_unwrapo!(v).parse()))))
                },
                "Placeholder" => Either::Right(Rc::new(Value::Placeholder)),
                "Nothing" => Either::Right(Rc::new(Value::Nothing)),
                "String" => Either::Right(Rc::new(Value::String(unescape_string(try_unwrapo!(v))))),
                "Builtin" => {
                    let builtin_name = try_unwrapo!(v);
                    Either::Right(match builtin_mapping.get_b(&builtin_name) {
                        Some(b) => Rc::new(Value::Builtin(*b)),
                        None => tree!("Deserialize Error: Builtin Missing" builtin_name !),
                    })
                },
                "Compound" => {
                    Either::Left(NodeState::Compound(try_unwrap!(try_unwrapo!(v).parse())))
                },
                s => return Err(EvalErr::Detailed(tree!("Unknown primitive:" s))),
            });
        }

        // Create all cons/compounds
        loop {
            let mut needs_more_iterations = false;
            let mut changed = false;
            let mut values_new = vec![];
            for (line_number, v) in values.iter().enumerate() {
                macro_rules! bounds_check {
                    ($i:expr) => {{
                        let i = $i;
                        match values.get(i) {
                            Some(vi) => vi,
                            None => return Err(EvalErr::Detailed(tree!({format!("Index out of bounds {}:{} at line {}", line!(), column!(), line_number)} {i as i64}))),
                        }
                    }};
                }
                values_new.push(match *v {
                    Either::Left(NodeState::Cons(l, r)) => {
                        match (bounds_check!(l), bounds_check!(r)) {
                            (&Either::Right(ref l), &Either::Right(ref r)) => {
                                changed = true;
                                Either::Right(Rc::new(Value::Cons(l.clone(), r.clone())))
                            },
                            _ => {
                                needs_more_iterations = true;
                                Either::Left(NodeState::Cons(l, r))
                            },
                        }
                    },
                    Either::Left(NodeState::Compound(c)) => {
                        match bounds_check!(c) {
                            &Either::Right(ref c) => {
                                changed = true;
                                Either::Right(Rc::new(Value::Compound(c.clone())))
                            },
                            _ => {
                                needs_more_iterations = true;
                                Either::Left(NodeState::Compound(c))
                            },
                        }
                    },
                    Either::Right(ref value) => Either::Right(value.clone()),
                });
            }
            values = values_new;
            if !needs_more_iterations { break; }
            if !changed { return Err(EvalErr::Detailed(tree!("Cycle detected"))); }
        }

        match values.into_iter().nth(0) {
            Some(Either::Right(value)) => Ok(value),
            None => Err(EvalErr::Detailed(tree!("Empty file"))),
            _ => unreachable!(),
        }
    })
}

fn deduplicate(node: &Rc<Value>) -> Rc<Value> {
    fn is(a: &Rc<Value>, b: &Rc<Value>) -> bool {
        &**a as *const Value as usize == &**b as *const Value as usize
    }
    fn helper(node: &Rc<Value>, seen: &mut ::std::collections::BTreeSet<Rc<Value>>, count: &mut usize) -> Rc<Value> {
        if let Some(already) = seen.get(node) {
            if !is(node, already) {
                *count = *count + 1;
            }
            return already.clone();
        }
        let deduped_children = match **node {
            Value::Cons(ref left, ref right) => {
                let newleft = helper(left, seen, count);
                let newright = helper(right, seen, count);
                if !is(left, &newleft) || !is(right, &newright) {
                    Rc::new(Value::Cons(newleft, newright))
                } else {
                    node.clone()
                }
            },
            Value::Compound(ref inner) => {
                let newinner = helper(inner, seen, count);
                if !is(inner, &newinner) {
                    Rc::new(Value::Compound(newinner))
                } else {
                    node.clone()
                }
            },
            _ => node.clone(),
        };
        seen.insert(deduped_children.clone());
        deduped_children
    }
    let mut seen = ::std::collections::BTreeSet::new();
    let mut count = 0;
    let result = helper(node, &mut seen, &mut count);
    println!("Removed {} duplicates", count);
    result
}

pub fn serialize(root: &Rc<Value>) -> String {
    use std::collections::HashMap;
    use std::collections::VecDeque;
    use std::fmt::Write;
    let root = &deduplicate(root);
    let mut heap = HashMap::new();
    let mut to_process: VecDeque<&Value> = VecDeque::new();
    let mut output = String::new();
    macro_rules! add_child {
        ($e:expr) => {{
            let v: &Value = $e;
            let address = v as *const Value as usize;
            if !heap.contains_key(&address) {
                let id = heap.len();
                heap.insert(address, id);
                to_process.push_back(v);
                id
            } else {
                *heap.get(&address).unwrap()
            }
        }}
    }
    add_child!(root);
    BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
        let builtin_mapping = builtin_mapping.borrow();
        while let Some(pointer) = to_process.pop_front() {
            let line = match pointer {
                &Value::Cons(ref left, ref right) => {
                    format!("Cons {} {}", add_child!(left), add_child!(right))
                },
                &Value::Boolean(b) => format!("Boolean {}", b),
                &Value::Placeholder => format!("Placeholder"),
                &Value::Number(n) => format!("Number {}", n),
                &Value::Nothing => format!("Nothing"),
                &Value::String(ref s) => format!("String {}", escape_string(s)),
                &Value::Builtin(b) => {
                    format!("Builtin {}", builtin_mapping.get_a(&b).unwrap_or(&::BUILTIN_MISSING_NAME))
                },
                &Value::Compound(ref value) => {
                    format!("Compound {}", add_child!(value))
                },
            };
            writeln!(output, "{}", line).unwrap();
        }
    });
    output
}

// S-expr

struct TT {
    size: usize,
    tree: TokenTree,
}
enum TokenTree {
    Node {
        open: &'static str,
        children: Vec<TT>,
        close: &'static str,
    },
    Unary {
        prefix: &'static str,
        child: Box<TT>,
    },
    Leaf(String)
}
impl TokenTree {
    fn build(self) -> TT {
        use self::TokenTree::*;
        let size = match self {
            Node { open, ref children, close } => open.len() + children.iter().map(|child| child.size).sum::<usize>() + (children.len() - 1) + close.len(),
            Unary { prefix, ref child } => prefix.len() + child.size,
            Leaf(ref s) => s.len()
        };
        TT { size: size, tree: self }
    }
}

fn to_token_tree(value: &Value, builtin_mapping: &onetoone::OneToOne<&'static str, Builtin>) -> TT {
    // TODO: Make this not recursive
    use self::TokenTree::*;
    let intermediate = match *value {
        Value::Boolean(b) => Leaf(format!("{}", b)),
        Value::Placeholder => Leaf(format!("!")),
        Value::Number(n) => Leaf(format!("{}", n)),
        Value::Nothing => Leaf(format!(".")),
        Value::String(ref s) => Leaf(format!("\"{}\"", escape_string(s))), // TODO escape " too
        Value::Builtin(b) => {
            Leaf(format!("*{}", builtin_mapping.get_a(&b).unwrap_or(&::BUILTIN_MISSING_NAME)))
        },
        Value::Cons(_, _) => {
            let mut children = vec![];
            let mut cons = value;
            while let Value::Cons(ref leftn, ref rightn) = *cons {
                cons = &*rightn;
                children.push(to_token_tree(&*leftn, builtin_mapping));
            }
            if let Value::Nothing = *cons {
                Node { open: "[", children: children, close: "]" }
            } else {
                children.push(to_token_tree(cons, builtin_mapping));
                Node { open: "(", children: children, close: ")" }
            }
        },
        Value::Compound(ref value) => {
            Unary { prefix: "$", child: Box::new(to_token_tree(&*value, builtin_mapping)) }
        },
    };
    intermediate.build()
}

fn format_token_tree(tree: &TT, column: usize, output: &mut String) {
    // http://picolisp.com/wiki/?prettyPrint
    // If tree is Unary, Leaf, or has a size <= single_line_size, then write it out on this line
    // otherwise, write out each child on its own line, all at the same column

    // TODO: Make this not recursive
    use std::fmt::Write;
    use self::TokenTree::*;
    let single_line_size = 30;
    match tree.tree {
        Node { open, ref children, close } => {
            if children.iter().map(|child| child.size).sum::<usize>() > single_line_size {
                write!(output, "{}", open).unwrap();
                let child_column = column + open.len();
                format_token_tree(&children[0], child_column, output);
                for child in children.iter().skip(1) {
                    write!(output, "\n").unwrap();
                    for _ in 0..child_column {
                        write!(output, " ").unwrap();
                    }
                    format_token_tree(child, child_column, output);
                }
                write!(output, "{}", close).unwrap();
            } else {
                write!(output, "{}", open).unwrap();
                let mut child_column = column + open.len();
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        write!(output, " ").unwrap();
                    }
                    format_token_tree(child, child_column, output);
                    child_column += 1 + child.size;
                }
                write!(output, "{}", close).unwrap();
            }
        },
        Unary { prefix, ref child } => {
            write!(output, "{}", prefix).unwrap();
            format_token_tree(child, column + prefix.len(), output);
        },
        Leaf(ref s) => {
            write!(output, "{}", s).unwrap();
        },
    }
}

pub fn format_sexpr(value: &Value) -> String {
    let token_tree = BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
        let builtin_mapping = &*builtin_mapping.borrow();
        to_token_tree(value, builtin_mapping)
    });
    let mut output = String::new();
    format_token_tree(&token_tree, 0, &mut output);
    output
}

// Parse s-expr
use std;
struct Parser<'a> {
    iter: std::iter::Peekable<std::str::Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn pop(&mut self) -> Option<char> {
        let n = self.iter.next();
        match n {
            Some('\n') => {
                self.line += 1;
                self.column = 1;
            },
            Some(_) => {
                self.column += 1;
            },
            _ => ()
        }
        n
    }
    fn peek(&mut self) -> Option<char> {
        self.iter.peek().cloned()
    }
    fn pos(&self) -> String {
        format!("At line {} column {}", self.line, self.column)
    }
    fn expect(&mut self, c: char) -> Result<(), String> {
        let actual = self.pop();
        if actual == Some(c) {
            Ok(())
        } else {
            Err(format!("{}: expected '{}' but found '{}'", self.pos(), c, repr(actual)))
        }
    }
    fn eat_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\n' {
                self.pop();
            } else {
                break;
            }
        }
    }
}

fn repr(c: Option<char>) -> String {
    match c {
        Some(c) => format!("{}", c),
        None => format!("EOF"),
    }
}

fn check_eof<T>(r: Option<T>) -> Result<T, String> {
    r.ok_or_else(|| format!("Unexpected EOF"))
}

fn parse_string(p: &mut Parser) -> Result<String, String> {
    assert_eq!(p.pop(), Some('"'));
    let mut acc = String::new();
    let mut escaped = false;
    loop {
        let c = check_eof(p.pop())?;
        if escaped {
            match c {
                'n' => acc.push('\n'),
                _ => acc.push(c),
            }
            escaped = false;
        } else {
            match c {
                '\\' => escaped = true,
                '"' => return Ok(acc),
                _ => acc.push(c),
            }
        }
    }
}

fn parse_number(p: &mut Parser) -> Result<i64, String> {
    let negative = if p.peek() == Some('-') {
        p.pop();
        true
    } else {
        false
    };
    let mut acc: i64 = 0;
    let mut first = true;
    loop {
        match p.peek() {
            Some(n@'0'...'9') => acc = acc * 10 + (n as i64 - '0' as i64),
            _ if !first => return Ok(if negative { -acc } else { acc }),
            c => return Err(format!("{}: expected '0'..'9' but found '{}'", p.pos(), repr(c))),
        }
        p.pop();
        first = false;
    }
}

fn parse_list(p: &mut Parser, nil_terminated: bool) -> Result<Rc<Value>, String> {
    let (s, e) = if nil_terminated { ('[', ']') } else { ('(', ')') };
    assert_eq!(p.pop(), Some(s));
    let mut elements = vec![];
    loop {
        p.eat_whitespace();
        match check_eof(p.peek())? {
            c if c == e => {
                p.pop();
                if nil_terminated { elements.push(Rc::new(Value::Nothing)); }
                return if elements.len() >= 2 {
                    let mut iter = elements.into_iter().rev();
                    let last = iter.next().unwrap();
                    Ok(iter.fold(last, |acc, x| Value::cons(x, acc)))
                } else {
                    Err(format!("{}: need at least two elements for a list, but have {}", p.pos(), elements.len()))
                }
            },
            _ => elements.push(parse_sexpr_inner(p)?),
        }
    }
}

fn parse_builtin(p: &mut Parser) -> Result<String, String> {
    assert_eq!(p.pop(), Some('*'));
    let mut acc = String::new();
    while let Some(c) = p.peek() {
        match c {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-' => {
                acc.push(c);
                p.pop();
            },
            _ => break,
        }
    }
    if acc.len() > 0 {
        Ok(acc)
    } else {
        Err(format!("{}: builtin doesn't have a valid identifier", p.pos()))
    }
}

fn parse_sexpr_inner(p: &mut Parser) -> Result<Rc<Value>, String> {
    match check_eof(p.peek())? {
        '"' => {
            Ok(Rc::new(Value::String(parse_string(p)?)))
        },
        '*' => {
            let builtin_name = parse_builtin(p)?;
            BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
                let builtin_mapping = builtin_mapping.borrow();
                Ok(match builtin_mapping.get_b(&builtin_name.as_str()) {
                    Some(b) => Rc::new(Value::Builtin(*b)),
                    None => tree!("Deserialize Error: Builtin Missing" {builtin_name.clone()} !),
                })
            })
        },
        '0'...'9' | '-' => {
            Ok(Rc::new(Value::Number(parse_number(p)?)))
        },
        't' => {
            p.expect('t')?;
            p.expect('r')?;
            p.expect('u')?;
            p.expect('e')?;
            Ok(Rc::new(Value::Boolean(true)))
        },
        'f' => {
            p.expect('f')?;
            p.expect('a')?;
            p.expect('l')?;
            p.expect('s')?;
            p.expect('e')?;
            Ok(Rc::new(Value::Boolean(false)))
        },
        '.' => {
            p.pop();
            Ok(Rc::new(Value::Nothing))
        },
        '!' => {
            p.pop();
            Ok(Rc::new(Value::Placeholder))
        },
        '(' => {
            parse_list(p, false)
        },
        '[' => {
            parse_list(p, true)
        },
        '$' => {
            p.pop();
            Ok(Rc::new(Value::Compound(parse_sexpr_inner(p)?)))
        },
        c => {
            Err(format!("{}: unexpected '{}'", p.pos(), c))
        }
    }
}

pub fn parse_sexpr(s: &str) -> EvalResult {
    let mut parser = Parser { iter: s.chars().peekable(), line: 1, column: 1 };
    parser.eat_whitespace();
    match parse_sexpr_inner(&mut parser) {
        Ok(val) => Ok(val),
        Err(err) => Err(EvalErr::Detailed(Rc::new(Value::String(err)))),
    }
}