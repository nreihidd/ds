use ::{wrong_shape, Value, EvalResult, EvalErr, simple_error};
use std::rc::Rc;
use std;
use serialize::{serialize, deserialize, BUILTIN_SERIALIZE_MAPPING};

pub fn greater_than(value: Rc<Value>) -> EvalResult {
    match_tree!(value;
        [{Value::Number(a)} {Value::Number(b)}] => Ok(Rc::new(Value::Boolean(a > b)))
    ).unwrap_or_else(|| wrong_shape("greater_than", value))
}
pub fn equal_to(value: Rc<Value>) -> EvalResult {
    fn are_equal(a: &Rc<Value>, b: &Rc<Value>) -> bool {
        if &**a as *const Value as usize == &**b as *const Value as usize {
            true
        } else {
            match (&**a, &**b) {
                (&Value::Number(a), &Value::Number(b)) => a == b,
                (&Value::String(ref a), &Value::String(ref b)) => a == b,
                (&Value::Boolean(a), &Value::Boolean(b)) => a == b,
                (&Value::Placeholder, &Value::Placeholder) => true,
                (&Value::Nothing, &Value::Nothing) => true,
                (&Value::Builtin(a), &Value::Builtin(b)) => a == b,
                (&Value::Cons(ref al, ref ar), &Value::Cons(ref bl, ref br)) => are_equal(al, bl) && are_equal(ar, br),
                (&Value::Compound(ref a), &Value::Compound(ref b)) => are_equal(a, b),
                _ => false
            }
        }
    }
    let (a, b) = uncons!(value);
    Ok(Rc::new(Value::Boolean(are_equal(a, b))))
}
pub fn multiply(value: Rc<Value>) -> EvalResult {
    match_tree!(value;
        [{Value::Number(a)} {Value::Number(b)}] => Ok(Rc::new(Value::Number(a * b)))
    ).unwrap_or_else(|| wrong_shape("multiply", value))
}
pub fn remainder(value: Rc<Value>) -> EvalResult {
    match_tree!(value;
        [{Value::Number(a)} {Value::Number(b)}] => Ok(Rc::new(Value::Number(a % b)))
    ).unwrap_or_else(|| wrong_shape("remainder", value))
}
pub fn add(value: Rc<Value>) -> EvalResult {
    match_tree!(value;
        [{Value::Number(a)} {Value::Number(b)}] => Ok(Rc::new(Value::Number(a + b)))
    ).unwrap_or_else(|| wrong_shape("add", value))
}
pub fn number_to_string(value: Rc<Value>) -> EvalResult {
    fn number_to_string_pretty(n: i64) -> String {
        use std::fmt::Write;
        if n == 0 { return "0".to_string(); }
        let mut s = String::new();
        let mut n = n;
        let negative = n < 0;
        if negative { n = -n; }
        let mut num_digits = 0;
        loop {
            if n == 0 {
                break;
            }

            let r = n % 10;
            let d = n / 10;

            if num_digits > 0 && num_digits % 3 == 0 {
                s += ",";
            }
            write!(s, "{}", r).unwrap();
            num_digits += 1;
            n = d;
        }
        if negative { s += "-"; }
        s.chars().rev().collect()
    }
    match *value {
        Value::Number(n) => Ok(Rc::new(Value::String(number_to_string_pretty(n)))),
        _ => Err(EvalErr::Type("number_to_string needs a number".to_string()))
    }
}
pub fn builtin_to_string(value: Rc<Value>) -> EvalResult {
    match *value {
        Value::Builtin(f) => {
            Ok(BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
                let builtin_mapping = builtin_mapping.borrow();
                Rc::new(Value::String(builtin_mapping.get_a(&f).unwrap_or(&::BUILTIN_MISSING_NAME).to_string()))
            }))
        },
        _ => Err(EvalErr::Type("builtin_to_string needs a builtin".to_string()))
    }
}

pub fn left(value: Rc<Value>) -> EvalResult {
    match *value {
        Value::Cons(ref a, _) => Ok(a.clone()),
        _ => Err(EvalErr::Detailed(Value::cons(Rc::new(Value::String(format!("Left takes cons, but got:"))), value.clone()))),
    }
}
pub fn right(value: Rc<Value>) -> EvalResult {
    match *value {
        Value::Cons(_, ref b) => Ok(b.clone()),
        _ => Err(EvalErr::Type(format!("Right takes cons"))),
    }
}

pub fn type_of(value: Rc<Value>) -> EvalResult {
    Ok(Rc::new(Value::String((match *value {
        Value::Cons(_, _) => "cons",
        Value::Compound(_) => "compound",
        Value::Builtin(_) => "builtin",
        Value::Boolean(_) => "boolean",
        Value::Nothing => "nothing",
        Value::Placeholder => "placeholder",
        Value::String(_) => "string",
        Value::Number(_) => "number",
    }).to_string())))
}

pub fn typed(value: Rc<Value>) -> EvalResult {
    let (t, v) = uncons!(value);
    Ok(Value::compound(t.clone(), v.clone()))
}
pub fn compound_inner(value: Rc<Value>) -> EvalResult {
    match *value {
        Value::Compound(ref inner) => Ok(inner.clone()),
        _ => simple_error("compound_inner takes a compound, but got:", value.clone()),
    }
}

pub fn load(value: Rc<Value>) -> EvalResult {
    use std::io::Read;
    let filename = value;
    try_match! { Value::String(ref s) = *filename => {
        let mut f = try!(std::fs::File::open(s).map_err(|e| EvalErr::Type(format!("{}", e))));
        let mut b = String::new();
        try!(f.read_to_string(&mut b).map_err(|e| EvalErr::Type(format!("{}", e))));
        deserialize(&b)
    }}
}
pub fn save(value: Rc<Value>) -> EvalResult {
    use std::io::Write;
    let (filename, value) = uncons!(value);
    try_match! { Value::String(ref s) = **filename => {
        let mut f = try!(std::fs::File::create(s).map_err(|e| EvalErr::Type(format!("{}", e))));
        let to_write = serialize(&value);
        try!(f.write(to_write.as_bytes()).map_err(|e| EvalErr::Type(format!("{}", e))));
        Ok(Rc::new(Value::String(to_write)))
    }}
}

pub fn value_eval_catch(v: Rc<Value>) -> EvalResult {
    match ::builtins::eval::value_eval(v) {
        Ok(v) => Ok(v),
        Err(e) => Ok(::error_to_value(e)),
    }
}

pub fn assoc_list_set_shadow(arg: Rc<Value>) -> EvalResult {
    match_tree!(arg; [v ([_ l])] => {
        Ok(Value::compound(::builtins::types::assoc_list_type(), Value::cons(v.clone(), l.clone())))
    }).unwrap_or_else(|| wrong_shape("assoc_list_set_shadow", arg))
}

pub fn assoc_list_lookup(arg: Rc<Value>) -> EvalResult {
    let (v, lc) = uncons!(arg);
    let (_, l) = uncompound!(lc);
    let mut n = l;
    while let Value::Cons(ref left, ref right) = **n {
        let (key, val) = uncons!(left);
        if let Value::Boolean(true) = *try!(equal_to(Value::cons(key.clone(), v.clone()))) {
            return Ok(val.clone());
        }
        n = right;
    }
    Err(EvalErr::Detailed(tree!(
       "Failed to find:"
       v
       "in"
       lc
    )))
}

pub fn assoc_list_lookup_or(arg: Rc<Value>) -> EvalResult {
    let (m, def) = uncons!(arg);
    let (v, lc) = uncons!(m);
    let (_, l) = uncompound!(lc);
    let mut n = l;
    while let Value::Cons(ref left, ref right) = **n {
        let (key, val) = uncons!(left);
        if let Value::Boolean(true) = *try!(equal_to(Value::cons(key.clone(), v.clone()))) {
            return Ok(val.clone());
        }
        n = right;
    }
    Ok(def.clone())
}

pub fn assoc_list_set(arg: Rc<Value>) -> EvalResult {
    match_tree!(arg; [[k v] ([_ l])] => {
        let mut left_list = Rc::new(Value::Nothing);
        let mut right_list = l;
        while let Value::Cons(ref left, ref right) = **right_list {
            let (key, _) = uncons!(left);
            right_list = right;
            if let Value::Boolean(true) = *try!(::builtins::functions::equal_to(Value::cons(key.clone(), k.clone()))) {
                break;
            }
            left_list = Value::cons(left.clone(), left_list);
        }
        let mut result = Value::cons(Value::cons(k.clone(), v.clone()), right_list.clone());
        let mut left_list = &left_list;
        while let Value::Cons(ref left, ref right) = **left_list {
            left_list = right;
            result = Value::cons(left.clone(), result);
        }
        Ok(Value::compound(::builtins::types::assoc_list_type(), result))
    }).unwrap_or_else(|| wrong_shape("assoc_list_set", arg))
}