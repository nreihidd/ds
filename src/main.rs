#[macro_use]
extern crate glium;
extern crate nalgebra as na;
extern crate image;
extern crate time;
extern crate rusttype;
#[macro_use]
extern crate lazy_static;

use rusttype::{Font, FontCollection};

mod render;
mod layout;
mod onetoone;

macro_rules! uncons { ($e:expr) => {{
    let e: &Rc<Value> = &$e;
    try!(::uncons(e).ok_or_else(|| EvalErr::Type(format!("Uncons error: {}:{}:{}", file!(), line!(), column!()))))
}}}
macro_rules! uncompound { ($e:expr) => {{
    let e: &Rc<Value> = &$e;
    uncons!(*try!(::uncompound(e).ok_or_else(|| EvalErr::Type(format!("Uncompound error: {}:{}:{}", file!(), line!(), column!())))))
}}}
macro_rules! try_match {
    {$p:pat = $e:expr => $b:expr} => { match $e { $p => $b, _ => return Err(::EvalErr::Type(format!("Match error: {}:{}", line!(), column!()))) } }
}

macro_rules! tree {
    ({$e:expr}) => ({use ::ToBValue; $e.to_bvalue()});
    (($($inner:tt)+)) => (tree!($($inner)+));
    ([$($inner:tt)+]) => (tree!($($inner)+ .));
    (.) => (Rc::new(Value::Nothing));
    (!) => (Rc::new(Value::Placeholder));
    (& $left:ident $($right:tt)*) => (tree!({&$left} $($right)*)); // To allow &ident for borrowing (so to_bvalue clones Rc) as a shorthand for {&ident}
    (* $left:tt $($right:tt)*) => (tree!({Rc::new(Value::Compound(tree!($left)))} $($right)*));
    (@ $left:tt $($right:tt)*) => (tree!({$left as ::Builtin} $($right)*)); // Needed because of https://github.com/rust-lang/rust/issues/21086
    ($left:tt) => ({use ::ToBValue; $left.to_bvalue()});
    ($left:tt $($right:tt)+) => (Value::cons(tree!($left), tree!($($right)+)));
}

macro_rules! match_tree_layer {
	(($i:ident [$left:tt $right:tt] $($stack:tt)*); $body:expr; $ri:ident) => {
		if let Value::Cons(ref l, ref r) = **$i {
			match_tree_layer!((l $left r $right $($stack)*); $body; $ri)
		}
	};
	(($i:ident ($inner:tt) $($stack:tt)*); $body:expr; $ri:ident) => {
		if let Value::Compound(ref inner) = **$i {
			match_tree_layer!((inner $inner $($stack)*); $body; $ri)
		}
	};
	(($i:ident {$pat:pat} $($stack:tt)*); $body:expr; $ri:ident) => {
		if let $pat = **$i {
			match_tree_layer!(($($stack)*); $body; $ri)
		}
	};
	(($i:ident _ $($stack:tt)*); $body:expr; $ri:ident) => {
        {
            let _ = $i;
		    match_tree_layer!(($($stack)*); $body; $ri)
        }
	};
	(($i:ident $b:ident $($stack:tt)*); $body:expr; $ri:ident) => {
		{
            let $b = $i;
			match_tree_layer!(($($stack)*); $body; $ri)
		}
	};
	((); $body:expr; $ri:ident) => {{
		$ri = Some($body);
		break;
	}};
}
macro_rules! match_tree {
	($e:expr; $($t:tt => $rhs:expr),+) => {{
		let mut r = None;
		let ref e = $e;
		loop {
		$(
			match_tree_layer!((e $t); $rhs; r);
		)+
		break;
		}
		r
	}};
	($e:expr; $($t:tt => $rhs:expr),+,) => {match_tree!($e; $($t => $rhs),+)};
}

mod builtins;
mod testdata;
mod serialize;

use std::rc::Rc;

use layout::{LayoutPath, LayoutNode};

lazy_static!{static ref FONT: Font<'static> = {
    use std::io::Read;
    let mut font_file = std::fs::File::open("Arial Unicode.ttf").unwrap();
    let mut font_data = Vec::new();
    font_file.read_to_end(&mut font_data).unwrap();
    let font = FontCollection::from_bytes(std::sync::Arc::new(font_data.into_boxed_slice())).into_font().unwrap();
    font
    // RefCell::new(Font::)
};}

fn uncons<'a>(value: &'a Rc<Value>) -> Option<(&'a Rc<Value>, &'a Rc<Value>)> {
    match **value {
        Value::Cons(ref left, ref right) => Some((left, right)),
        _ => None
    }
}

fn uncompound<'a>(value: &'a Rc<Value>) -> Option<&'a Rc<Value>> {
    match **value {
        Value::Compound(ref inner) => Some(inner),
        _ => None,
    }
}


fn simple_error(description: &str, value: Rc<Value>) -> EvalResult {
    Err(EvalErr::Detailed(Value::cons(Rc::new(Value::String(description.to_string())), value)))
}

fn wrong_shape(name: &str, val: Rc<Value>) -> EvalResult {
    let mut s = name.to_string();
    s.push_str(" has the wrong shape");
    Err(EvalErr::Detailed(tree!(s {val})))
}

pub type Builtin = fn(Rc<Value>) -> EvalResult;

pub type EvalResult = Result<Rc<Value>, EvalErr>;
#[derive(Debug)]
pub enum EvalErr {
    Type(String),
    Load(String),
    Placeholder,
    Detailed(Rc<Value>),
}

pub type BValue = Rc<Value>;
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Placeholder, // Maybe Compound(PLACEHOLDER, Nothing) instead
    Number(i64),
    String(String),
    Boolean(bool),
    Cons(BValue, BValue),
    Nothing, // Only use this as the empty list, do not use it like null; should maybe be Compound(NOTHING, Placeholder)?

    Builtin(Builtin),
    Compound(BValue),
}

trait ToBValue {
    fn to_bvalue(self) -> Rc<Value>;
}
impl ToBValue for i64 {
    fn to_bvalue(self) -> Rc<Value> {
        Rc::new(Value::Number(self))
    }
}
impl ToBValue for bool {
    fn to_bvalue(self) -> Rc<Value> {
        Rc::new(Value::Boolean(self))
    }
}
impl<'a> ToBValue for &'a str {
    fn to_bvalue(self) -> Rc<Value> {
        Rc::new(Value::String(self.to_string()))
    }
}
impl ToBValue for String {
    fn to_bvalue(self) -> Rc<Value> {
        Rc::new(Value::String(self))
    }
}
impl ToBValue for () {
    fn to_bvalue(self) -> Rc<Value> {
        Rc::new(Value::Nothing)
    }
}
impl ToBValue for Builtin {
    fn to_bvalue(self) -> Rc<Value> {
        Rc::new(Value::Builtin(self))
    }
}
impl ToBValue for Rc<Value> {
    fn to_bvalue(self) -> Rc<Value> {
        self
    }
}
impl<'a> ToBValue for &'a Rc<Value> {
    fn to_bvalue(self) -> Rc<Value> {
        self.clone()
    }
}

impl Value {
    fn compound(dict: Rc<Value>, value: Rc<Value>) -> Rc<Value> {
        Rc::new(Value::Compound(Value::cons(dict, value)))
    }
    fn cons(left: Rc<Value>, right: Rc<Value>) -> Rc<Value> {
        Rc::new(Value::Cons(left, right))
    }
}
pub const BUILTIN_MISSING_NAME: &'static str = "missingname";
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Value::Placeholder => write!(f, "Placeholder"),
            Value::Number(n) => write!(f, "Number {}", n),
            Value::String(ref s) => write!(f, "String {:?}", s),
            Value::Boolean(b) => write!(f, "Boolean {}", b),
            Value::Cons(ref left, ref right) => write!(f, "({:?} {:?})", left, right),
            Value::Nothing => write!(f, "Nothing"),
            Value::Builtin(func) => write!(f, "Builtin {}", serialize::BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
                builtin_mapping.borrow().get_a(&func).map(|name| name.to_string()).unwrap_or_else(|| BUILTIN_MISSING_NAME.to_string())
            })),
            Value::Compound(ref value) => write!(f, "Compound {:?}", value),
        }
    }
}

#[derive(Debug)]
enum Direction {
    Left,
    Right,
    Value,
}

fn path_to_directions(path: &Rc<LayoutPath>) -> Vec<Direction> {
    use layout::LayoutPath::*;
    let mut v = vec![];
    let mut path = path;
    loop {
        match &**path {
            &End => return v,
            &Left(ref p) =>  { v.push(Direction::Left);  path = p; },
            &Right(ref p) => { v.push(Direction::Right); path = p; },
            &Value(ref p) => { v.push(Direction::Value); path = p; },
        }
    }
}

fn directions_to_path(dirs: &[Direction]) -> Rc<LayoutPath> {
    use layout::LayoutPath::*;
    let mut n = Rc::new(LayoutPath::End);
    for dir in dirs.iter().rev() {
        n = Rc::new(match *dir {
            Direction::Left => LayoutPath::Left(n),
            Direction::Right => LayoutPath::Right(n),
            Direction::Value => LayoutPath::Value(n),
        })
    }
    n
}

fn navigate_replace(start: &Rc<Value>, path: &[Direction], to_insert: Rc<Value>) -> Rc<Value> {
    if path.len() == 0 {
        to_insert
    } else {
        match (&path[0], &**start) {
            (&Direction::Left, &Value::Cons(ref left, ref right)) => Rc::new(Value::Cons(navigate_replace(left, &path[1..], to_insert), right.clone())),
            (&Direction::Right, &Value::Cons(ref left, ref right)) => Rc::new(Value::Cons(left.clone(), navigate_replace(right, &path[1..], to_insert))),
            (&Direction::Value, &Value::Compound(ref value)) => Rc::new(Value::Compound(navigate_replace(value, &path[1..], to_insert))),
            (dir, _) => { println!("Took a wrong {:?} turn while replacing", dir); to_insert },
        }
    }
}

fn navigate(start: &Rc<Value>, path: &[Direction]) -> Rc<Value> {
    let mut val = start;
    for dir in path {
        val = match (dir, &**val) {
            (&Direction::Left, &Value::Cons(ref left, _)) => left,
            (&Direction::Right, &Value::Cons(_, ref right)) => right,
            (&Direction::Value, &Value::Compound(ref value)) => value,
            _ => { println!("Took a wrong {:?} turn", dir); val },
        }
    }
    val.clone()
}

fn value_tooltip(v: &Rc<Value>, global_env: &Rc<Value>) -> Rc<Value> {
    match_tree!(*v; ([{Value::String(ref s)} val]) => {
        if s == "load" {
            if let Ok(global_val) = builtins::functions::assoc_list_lookup(tree!({val} {global_env})) {
                Some(global_val)
            } else {
                None
            }
        } else {
            None
        }
    }).unwrap_or(None).unwrap_or_else(|| Rc::new(Value::Placeholder))
}

fn error_to_value(e: EvalErr) -> Rc<Value> {
    match e {
        EvalErr::Detailed(contents) => Rc::new(Value::Cons(Rc::new(Value::String("Error".to_string())), contents)),
        EvalErr::Type(s) | EvalErr::Load(s) => Rc::new(Value::Cons(Rc::new(Value::String("Error".to_string())), Rc::new(Value::String(s)))),
        EvalErr::Placeholder => Rc::new(Value::String("Placeholder error".to_string())),
    }
}

fn result_to_value(r: EvalResult) -> Rc<Value> {
    match r {
        Ok(v) => v,
        Err(e) => error_to_value(e),
    }
}

static mut global_want_tree_layout: bool = false;

struct LayoutCacheEntry {
    layout: LayoutNode,
    geometry: Option<render::Geometry>,
}
struct LayoutCache {
    map: std::collections::HashMap<Rc<Value>, LayoutCacheEntry>,
}

impl LayoutCache {
    fn new() -> LayoutCache {
        LayoutCache {
            map: std::collections::HashMap::new(),
        }
    }
    fn clear_except(&mut self, values: &[Rc<Value>]) {
        use std::iter::FromIterator;
        let to_preserve = std::collections::HashSet::<&Rc<Value>>::from_iter(values);
        let m = self.map.drain().filter(|x| to_preserve.contains(&x.0)).collect();
        self.map = m;
    }
    fn get_entry(&mut self, value: Rc<Value>, global_env: Rc<Value>) -> &mut LayoutCacheEntry {
        self.map.entry(value.clone()).or_insert_with(|| {
            LayoutCacheEntry {
                layout: do_layout(&value, &global_env),
                geometry: None,
            }
        })
    }
    fn get_layout(&mut self, value: Rc<Value>, global_env: Rc<Value>) -> &LayoutNode {
        &self.get_entry(value, global_env).layout
    }
    fn get_geometry(&mut self, value: Rc<Value>, context: &Rc<glium::backend::Context>, renderer: &mut render::DynamicRenderer, global_env: Rc<Value>) -> &render::Geometry {
        let entry = self.get_entry(value, global_env);
        if let None = entry.geometry {
            let mut builder = render::GeometryBuilder::new(renderer);
            entry.layout.draw(na::Vector2::new(0.0, 0.0), &mut builder);
            entry.geometry = Some(builder.build(context));
        }
        &entry.geometry.as_ref().unwrap()
    }
}

fn do_layout(v: &Rc<Value>, global_env: &Rc<Value>) -> layout::LayoutNode {
    use layout::Layout;
    macro_rules! tryo {
        ($e:expr) => { match $e { Some(r) => r, None => return None } }
    };
    fn make_layout(v: &Rc<Value>) -> layout::LayoutNode {
        if let Some(l) = maybe_make_layout(v) {
            l
        } else {
            LayoutNode::new(layout::make_text("Layout Error", na::Vector4::new(1.0, 0.0, 0.0, 1.0)))
        }
    }
    fn maybe_make_layout(v: &Rc<Value>) -> Option<layout::LayoutNode> {
        fn nth(v: &Rc<Value>, n: u32) -> Option<&Rc<Value>> {
            let mut pos = v;
            for _ in 0..n {
                pos = tryo!(uncons(pos)).1
            }
            Some(tryo!(uncons(pos)).0)
        }
        fn val_to_size(v: &Rc<Value>) -> Option<na::Vector2<f32>> {
            let (x, y) = tryo!(uncons(v));
            match (&**x, &**y) {
                (&Value::Number(x), &Value::Number(y)) => Some(na::Vector2::new(x as f32, y as f32)),
                _ => None,
            }
        }
        fn val_to_color(v: &Rc<Value>) -> Option<na::Vector4<f32>> {
            fn as_number(v: &Rc<Value>) -> Option<i64> {
                if let Value::Number(n) = **v {
                    Some(n)
                } else { None }
            }
            fn n2c(n: i64) -> f32 {
                n as f32 / 255.0
            }
            fn as_c(v: &Rc<Value>) -> Option<f32> {
                Some(n2c(tryo!(as_number(v))))
            }
            let (r, g, b, a) = (tryo!(nth(v, 0).and_then(as_c)), tryo!(nth(v, 1).and_then(as_c)), tryo!(nth(v, 2).and_then(as_c)), tryo!(nth(v, 3).and_then(as_c)));
            Some(na::Vector4::new(r, g, b, a))
        }
        fn val_to_string(v: &Rc<Value>) -> Option<String> {
            if let Value::String(ref s) = **v { Some(s.clone()) } else { None }
        }

        let (name, args) = tryo!(uncons(v));
        if let Value::String(ref name) = **name {
            Some(LayoutNode::new(match name.as_str() {
                "empty" => Layout::Empty(tryo!(val_to_size(tryo!(nth(args, 0))))),
                "background" => Layout::Background(Box::new(make_layout(tryo!(nth(args, 0)))), tryo!(val_to_color(tryo!(nth(args, 1))))),
                "text" => layout::make_text(&tryo!(val_to_string(tryo!(nth(args, 0)))), tryo!(val_to_color(tryo!(nth(args, 1))))),
                "border" => Layout::Border(Box::new(make_layout(tryo!(nth(args, 0)))), tryo!(val_to_size(tryo!(nth(args, 1)))), tryo!(val_to_color(tryo!(nth(args, 2))))),
                "horizontal" => Layout::Horizontal(Box::new(make_layout(tryo!(nth(args, 0)))), Box::new(make_layout(tryo!(nth(args, 1))))),
                "dbgtree" => Layout::DbgTreeVertical(Box::new(make_layout(tryo!(nth(args, 0)))), Box::new(make_layout(tryo!(nth(args, 1))))),
                "vertical" => Layout::Vertical(Box::new(make_layout(tryo!(nth(args, 0)))), Box::new(make_layout(tryo!(nth(args, 1)))), 0.5),
                "vertical-left" => Layout::Vertical(Box::new(make_layout(tryo!(nth(args, 0)))), Box::new(make_layout(tryo!(nth(args, 1)))), 0.0),
                "table" => {
                    let layouts = list_to_vec(args).iter().map(|v| list_to_vec(v).iter().map(|e| make_layout(e)).collect()).collect();
                    Layout::Table(layouts)
                },
                "navleft" => Layout::NavLeft(Box::new(make_layout(tryo!(nth(args, 0))))),
                "navright" => Layout::NavRight(Box::new(make_layout(tryo!(nth(args, 0))))),
                "navcompound" => Layout::NavCompound(Box::new(make_layout(tryo!(nth(args, 0))))),
                _ => return None,
            }))
        } else {
            None
        }
    }
    let run_layout = |to_layout: Rc<Value>| {
        if unsafe { global_want_tree_layout } {
            builtins::layout::tree_layout(to_layout)
        } else {
            builtins::functions::assoc_list_lookup(tree!("layout" {global_env})).and_then(|layout_func| {
                match *layout_func {
                    Value::Builtin(b) => b(to_layout),
                    _ => builtins::eval::value_eval(tree!(
                        *("apply_closure" *("quote" {to_layout} {layout_func}))
                        . {global_env}
                    ))
                }
            })
        }
    };
    let r = run_layout(Value::cons(v.clone(), Rc::new(Value::Nothing)))
        .or_else(|e| run_layout(tree!("Layout Error" {error_to_value(e)} .)));
    match r {
        Ok(r) => LayoutNode::new(Layout::Background(Box::new(make_layout(&r)), na::Vector4::new(1.0, 1.0, 1.0, 1.0))),
        Err(e) => {
            println!("Layout Error: {:?}", e);
            LayoutNode::new(layout::make_text("Layout Double Error", na::Vector4::new(1.0, 0.0, 0.0, 1.0)))
        }
    }
}

fn list_to_vec(list: &Rc<Value>) -> Vec<Rc<Value>> {
    let mut n = list;
    let mut v = vec![];
    while let Value::Cons(ref left, ref right) = **n {
        v.push(left.clone());
        n = right;
    }
    v
}
fn slice_to_list(slice: &[Rc<Value>]) -> Rc<Value> {
    slice.iter().rev().fold(Rc::new(Value::Nothing), |acc, v| {
        Value::cons(v.clone(), acc)
    })
}

fn do_toplevel<F: FnMut(Rc<Value>, f32, f32) -> Option<Rc<Value>>>(global_env: Rc<Value>, mut f: F) -> Rc<Value> {
    let global_env = &global_env;
    let workspace_str = &Rc::new(Value::String("workspace".to_string()));
    let workspace_old = ::builtins::functions::assoc_list_lookup(tree!({workspace_str} {global_env})).unwrap_or_else(|_| tree!(*("assoc_list" .)));
    let workspace_entries = list_to_vec(&workspace_old).into_iter().filter_map(|val| {
        if let Value::Cons(ref val, ref coord) = *val {
            if let Value::Cons(ref x, ref y) = **coord {
                if let (&Value::Number(x), &Value::Number(y)) = (&**x, &**y) {
                    return f(val.clone(), x as f32, y as f32).map(|v| Value::cons(v, coord.clone()));
                }
            }
        }
        None
    }).collect::<Vec<_>>();
    let workspace_new = slice_to_list(&workspace_entries);
    ::builtins::functions::assoc_list_set(tree!(({workspace_str} {workspace_new}) {global_env})).unwrap()
}

struct History {
    undo_stack: Vec<Rc<Value>>,
    redo_stack: Vec<Rc<Value>>,
}
impl History {
    fn new() -> History {
        History { undo_stack: vec![], redo_stack: vec![] }
    }
    fn undo(&mut self, current: Rc<Value>) -> Rc<Value> {
        match self.undo_stack.pop() {
            Some(g) => {
                self.redo_stack.push(current);
                g
            },
            None => current,
        }
    }
    fn redo(&mut self, current: Rc<Value>) -> Rc<Value> {
        match self.redo_stack.pop() {
            Some(g) => {
                self.undo_stack.push(current);
                g
            },
            None => current,
        }
    }
    fn add(&mut self, state: Rc<Value>) {
        self.redo_stack.clear();
        self.undo_stack.push(state);
    }
}

fn window_to_world(window_to_gl: na::Matrix4<f32>, world_to_gl: na::Matrix4<f32>) -> na::Matrix4<f32> {
    use na::Inverse;
    world_to_gl.inverse().unwrap() * window_to_gl
}
fn mat_transform_2d(matrix: na::Matrix4<f32>, point: na::Vector2<f32>) -> na::Vector2<f32> {
    let r = matrix * na::Vector4::new(point.x, point.y, 0.0, 1.0);
    na::Vector2::new(r.x, r.y)
}

fn orthographic(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) -> na::Matrix4<f32> {
    // Because na::OrthographicMatrix3 asserts right > left and top > bottom for some reason
    // https://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml
    let width = right - left;
    let height = top - bottom;
    let depth = far - near;
    na::Matrix4::new(
        2.0 / width, 0.0, 0.0, -(right + left) / width,
        0.0, 2.0 / height, 0.0, -(top + bottom) / height,
        0.0, 0.0, -2.0 / depth, -(far + near) / depth,
        0.0, 0.0, 0.0, 1.0
    )
}

fn skip_n(n: usize, dirs: &[Direction]) -> &[Direction] {
    let l = dirs.len();
    if n > l { &[] }
    else { &dirs[..l - n] }
}

fn main() {
    use glium::glutin::VirtualKeyCode as VK;
    use glium::DisplayBuild;
    use glium::Surface;
    use glium::backend::Facade;
    use std::collections::HashSet;

    let display = glium::glutin::WindowBuilder::new()
        .with_dimensions(1024, 768)
        .with_title(format!("Hello world"))
        .build_glium()
        .unwrap();
    let ref context = display.get_context().clone();
    let mut geometry = render::DynamicRenderer::new(context);
    let mut top_num = 0i64;

    let mut scroll_offset = na::Vector2::new(0.0, 0.0);
    let mut zoom_power = 0.0;

    serialize::BUILTIN_SERIALIZE_MAPPING.with(|m| {
        use builtins::eval::*;
        use builtins::layout::*;
        use builtins::functions::*;
        let mut m = m.borrow_mut();
        macro_rules! insert_m {
            ($($i:ident),*) => { $(m.insert(stringify!($i), $i);)* }
        }
        insert_m!(
            value_eval, typed, greater_than, equal_to, multiply,
            remainder, add, left, right, compound_inner, load, save, type_of, value_eval_catch,
            assoc_list_lookup, assoc_list_lookup_or, assoc_list_set, assoc_list_set_shadow, number_to_string,
            tree_layout, builtin_to_string
        );
    });

    let mut global_env = tree!(*("assoc_list" [
        ("eval_dict" *("assoc_list" .))
        ("shortcuts_dict" *("assoc_list" .))
        ("workspace" .)
        ("layout" @{::builtins::layout::tree_layout})
    ]));

    let (results, basic_env, look_closure) = testdata::test_evals(&global_env);

    let mut layout_cache = LayoutCache::new();

    let examples = tree!([
        ({results} 400 100)
        ({basic_env} 100 300)
        ({look_closure} 1400 100)
        (top_num 200 100)
        ("バナナ" 200 300)
        ("testhing.ds" 200 200)
        ({&global_env} 200 200)
        ({serialize::BUILTIN_SERIALIZE_MAPPING.with(|m| m.borrow().b().keys().fold(Rc::new(Value::Nothing), |acc, f| {
            Rc::new(Value::Cons(Rc::new(Value::Builtin(*f)), acc))
        }))} 0 500)
    ]);
    let workspace_str = Rc::new(Value::String("workspace".to_string()));
    global_env = ::builtins::functions::assoc_list_set(tree!(({&workspace_str} {examples}) {&global_env})).unwrap();

    let mut world_to_gl: na::Matrix4<f32> = na::Eye::new_identity(4);
    let mut window_to_gl: na::Matrix4<f32> = na::Eye::new_identity(4);

    let mut window_hover_pos = na::Vector2::new(0.0, 0.0);
    let mut hover_depth: i32 = 0;
    let mut control_groups: Vec<Rc<Value>> = std::iter::repeat(Rc::new(Value::Placeholder)).take(10).collect();
    let mut clipboard = Rc::new(Value::Placeholder);
    let empty_environment = Value::compound(builtins::types::assoc_list_type(), Rc::new(Value::Nothing));

    let mut keys_held = HashSet::new();

    let mut text_entry = false;

    let mut world_panning_anchor: Option<na::Vector2<f32>> = None;
    struct Grab {
        pre_grab_global_env: Rc<Value>,
        world_mouse_offset: na::Vector2<f32>,
    }
    let mut grab: Option<Grab> = None;

    let mut history = History::new();
    macro_rules! update_global_env {
        ($e:expr) => {{
            let new_env = $e;
            if new_env != global_env {
                history.add(global_env);
                global_env = new_env;
            }
        }};
    }

    fn make_env(global_env: &Rc<Value>) -> Rc<Value> {
        tree!(*("assoc_list" .) {&global_env})
    }

	'outer: loop {
        let frame_start = time::precise_time_ns();
		for event in display.poll_events() {
			match event {
				glium::glutin::Event::Closed => break 'outer,
                glium::glutin::Event::KeyboardInput(glium::glutin::ElementState::Released, _, Some(key)) => {
                    keys_held.remove(&key);
                    match key {
                        _ => (),
                    }
                },
                glium::glutin::Event::ReceivedCharacter(c) if text_entry => {
                    if c != '\u{1b}' { // Don't insert escape
                        let mut s = if let Value::String(ref s) = *clipboard {
                            s.clone()
                        } else {
                            String::new()
                        };
                        if c == '\u{8}' {
                            s.pop();
                        } else {
                            s.push(c);
                        }
                        clipboard = Rc::new(Value::String(s));
                    }
                },
                glium::glutin::Event::ReceivedCharacter(c) if !text_entry => {
                    let mut s = String::new();
                    s.push(c);
                    let lookup_shortcut = tree!(
                        *("apply_builtin" (s *("load" "shortcuts_dict")) @{builtins::functions::assoc_list_lookup})
                        &empty_environment &global_env
                    );
                    if let Ok(v) = builtins::eval::value_eval(lookup_shortcut) {
                        let r = ::builtins::eval::value_eval(tree!(*("apply_closure" &clipboard v) &empty_environment)); // THIS LOOKS WRONG, MISSING GLOBAL_ENV
                        clipboard = result_to_value(r);
                    }
                },
                glium::glutin::Event::KeyboardInput(glium::glutin::ElementState::Pressed, _, Some(VK::Escape)) if text_entry => {
                    text_entry = false;
                },
                glium::glutin::Event::KeyboardInput(glium::glutin::ElementState::Pressed, _, Some(key)) if !text_entry => {
                    keys_held.insert(key);
                    match key {
                        // Load & Store control groups // TODO: Remove these or make them always visible? I never ever use them as it is now.
                        VK::Key1 | VK::Key2 | VK::Key3 | VK::Key4 | VK::Key5 | VK::Key6 | VK::Key7 | VK::Key8 | VK::Key9 => {
                            let index = match key { VK::Key1 => 0, VK::Key2 => 1, VK::Key3 => 2, VK::Key4 => 3, VK::Key5 => 4, VK::Key6 => 5, VK::Key7 => 6, VK::Key8 => 7, VK::Key9 => 8, _ => unreachable!() };
                            if keys_held.contains(&VK::LControl) {
                                control_groups[index] = clipboard.clone();
                            } else {
                                clipboard = control_groups[index].clone();
                            }
                        },
                        // Eval hovered value and put the result on the clipboard
                        VK::E => {
                            let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                            do_toplevel(global_env.clone(), |val, x, y| {
                                let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                                if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                    let target = navigate(&val, skip_n(hover_depth as usize, &path_to_directions(&path)));
                                    let new_clipboard = builtins::eval::value_eval(Value::cons(target, make_env(&global_env))).unwrap_or_else(error_to_value);
                                    clipboard = new_clipboard;
                                }
                                None
                            });
                        },
                        // Enter string entry mode
                        VK::Return => {
                            text_entry = true;
                            if let Value::String(_) = *clipboard { } else {
                                clipboard = Rc::new(Value::String(String::new()));
                            };
                        },
                        // Copy to system clipboard
                        VK::C if keys_held.contains(&VK::LControl) => {
                            use std::process::{Command, Stdio};
                            use std::io::Write;
                            let text = format!("{:?}", clipboard);
                            let mut child = Command::new("xsel").arg("--clipboard").stdin(Stdio::piped()).spawn().unwrap();
                            println!("Putting {} on the clipboard", text);
                            child.stdin.as_mut().map(|mut stdin| write!(stdin, "{}", text));
                            child.wait().unwrap();
                        },
                        // Paste from system clipboard
                        VK::V if keys_held.contains(&VK::LControl) => {
                            use std::process::{Command, Stdio};
                            use std::io::Read;
                            let mut child = Command::new("xsel").arg("--clipboard").stdout(Stdio::piped()).spawn().unwrap();
                            println!("Getting the clipboard");
                            let mut text = String::new();
                            child.stdout.as_mut().map(|mut stdout| stdout.read_to_string(&mut text)).map(|_| {
                                clipboard = Rc::new(Value::String(text));
                            });
                            child.wait().unwrap();
                        },
                        // Save global_env to clipboard path
                        VK::S if keys_held.contains(&VK::LControl) && keys_held.contains(&VK::LShift) => {
                            let _ = builtins::functions::save(Value::cons(
                                clipboard.clone(),
                                global_env.clone()
                            )).map_err(|_e| println!("Error saving"));
                        },
                        // Save global_env to session.ds
                        VK::S if keys_held.contains(&VK::LControl) => {
                            let _ = builtins::functions::save(Value::cons(
                                Rc::new(Value::String("session.ds".to_string())),
                                global_env.clone()
                            )).map_err(|_e| println!("Error saving"));
                        },
                        // Restore global_env from clipboard path
                        VK::O if keys_held.contains(&VK::LControl) && keys_held.contains(&VK::LShift) => {
                            update_global_env!(builtins::functions::load(clipboard.clone()).map_err(|e| println!("Error loading: {:?}", e)).unwrap_or(global_env.clone()));
                        },
                        // Restore global_env from session.ds
                        VK::O if keys_held.contains(&VK::LControl) => {
                            update_global_env!(builtins::functions::load(Rc::new(Value::String("session.ds".to_string()))).map_err(|e| println!("Error loading: {:?}", e)).unwrap_or(global_env.clone()));
                        },
                        // Redo
                        VK::Y if keys_held.contains(&VK::LControl) => global_env = history.redo(global_env),
                        VK::Z if keys_held.contains(&VK::LControl) && keys_held.contains(&VK::LShift) => global_env = history.redo(global_env),
                        // Undo
                        VK::Z if keys_held.contains(&VK::LControl) => global_env = history.undo(global_env),
                        // Re-layout all values
                        VK::F5 => {
                            layout_cache.clear_except(&[]);
                        },
                        // Zoom
                        VK::Subtract => zoom_power -= 1.0,
                        VK::Equals => zoom_power += 1.0,
                        // Delete hovered value
                        VK::Delete => {
                            let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                            update_global_env!(do_toplevel(global_env.clone(), |val, x, y| {
                                let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                                if let Some(_) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                    None
                                } else {
                                    Some(val)
                                }
                            }));
                        },
                        // Quote the hovered value
                        VK::Q => {
                            let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                            update_global_env!(do_toplevel(global_env.clone(), |val, x, y| {
                                let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                                if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                    let path = &path_to_directions(&path);
                                    let replacement = Value::compound(builtins::types::quote_type(), navigate(&val, path));
                                    Some(navigate_replace(&val, path, replacement))
                                } else {
                                    Some(val)
                                }
                            }));
                        },
                        // Using the clipboard as a template, replace the hovered value with the template with the template's first placeholder replaced by the hovered value
                        VK::I => {
                            let template = &clipboard;
                            fn path_to_first_placeholder(template: &Rc<Value>) -> Option<Rc<LayoutPath>> {
                                match **template {
                                    Value::Cons(ref left, ref right) => {
                                        path_to_first_placeholder(left)
                                            .map(|p| Rc::new(LayoutPath::Left(p)))
                                            .or_else(|| path_to_first_placeholder(right)
                                                            .map(|p| Rc::new(LayoutPath::Right(p))))
                                    }
                                    Value::Compound(ref inner) => path_to_first_placeholder(inner).map(|p| Rc::new(LayoutPath::Value(p))),
                                    Value::Placeholder => Some(Rc::new(LayoutPath::End)),
                                    _ => None,
                                }
                            }
                            let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                            if let Some(template_path) = path_to_first_placeholder(template) {
                                update_global_env!(do_toplevel(global_env.clone(), |val, x, y| {
                                    let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                                    if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                        let path = &path_to_directions(&path);
                                        let hit_value = navigate(&val, path);
                                        let replacement = navigate_replace(&template, &path_to_directions(&template_path), hit_value);
                                        Some(navigate_replace(&val, path, replacement))
                                    } else {
                                        Some(val)
                                    }
                                }));
                            }
                        },
                        // Toggle tree layout
                        VK::T => {
                            unsafe {
                                global_want_tree_layout = !global_want_tree_layout;
                            }
                            layout_cache.clear_except(&[]);
                        },
                        // Store to global_env
                        VK::G => {
                            let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                            let mut hovered_value = None;
                            do_toplevel(global_env.clone(), |val, x, y| {
                                let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                                if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                    hovered_value = Some(navigate(&val, skip_n(hover_depth as usize, &path_to_directions(&path))));
                                }
                                None
                            });
                            if let Some(hovered_value) = hovered_value {
                                let result = if keys_held.contains(&VK::LControl) {
                                    ::builtins::eval::value_eval(tree!(
                                        {hovered_value}
                                        *("assoc_list" .) {&global_env}
                                    ))
                                } else {
                                    ::builtins::eval::value_eval(tree!(
                                        *("apply_builtin" ((*("quote" &clipboard) hovered_value) *("quote" &global_env)) @{::builtins::functions::assoc_list_set})
                                        *("assoc_list" .) &global_env
                                    ))
                                };
                                match result {
                                    Ok(new_global) => update_global_env!(new_global),
                                    Err(err) => clipboard = error_to_value(err),
                                }
                            }
                        },
                        VK::P => {
                            if keys_held.contains(&VK::LControl) {
                                // Parse an sexpr representation from the clipboard
                                clipboard = match *clipboard {
                                    Value::String(ref s) => {
                                        match serialize::parse_sexpr(s) {
                                            Ok(val) => val,
                                            Err(err) => error_to_value(err),
                                        }
                                    },
                                    _ => { println!("Can't parse a non-string"); clipboard.clone() },
                                }
                            } else {          
                                // Print out an sexpr representation of the clipboard                      
                                println!("{}", serialize::format_sexpr(&clipboard));
                            }
                        },
                        _ => (),
                    }
                },
                // Update hovered position
                glium::glutin::Event::MouseMoved(x, y) => {
                    window_hover_pos = na::Vector2::new(x as f32, y as f32);
                },
                // Grab
                glium::glutin::Event::MouseInput(glium::glutin::ElementState::Pressed, glium::glutin::MouseButton::Right) if keys_held.contains(&VK::LControl) => {
                    let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                    // Walk workspace, find first item that the mouse hit (front-most);
                    // Construct new workspace with dragged item at head,
                    match builtins::functions::assoc_list_lookup(tree!(&workspace_str &global_env)) {
                        Ok(workspace) => {
                            let mut n = &workspace;
                            let mut to_shift: Vec<&Rc<Value>> = vec![];
                            'grabloop: loop {
                                if match_tree!(*n; [entry rest] => {
                                    match_tree!(*entry; [item [{Value::Number(x)} {Value::Number(y)}]] => {
                                        let layout = layout_cache.get_layout(item.clone(), global_env.clone());
                                        let item_pos = na::Vector2::new(x as f32, y as f32);
                                        if let Some(_) = layout.hit(item_pos, world_hover_pos) {
                                            let mut new_workspace = rest.clone();
                                            for shifted in to_shift.into_iter().rev() {
                                                new_workspace = Value::cons(shifted.clone(), new_workspace);
                                            }
                                            new_workspace = Value::cons(entry.clone(), new_workspace);
                                            grab = Some(Grab { world_mouse_offset: item_pos - world_hover_pos, pre_grab_global_env: global_env.clone() });
                                            global_env = ::builtins::functions::assoc_list_set(tree!((&workspace_str new_workspace) &global_env)).unwrap();
                                            break 'grabloop;
                                        } else {
                                            to_shift.push(entry);
                                        }
                                    });
                                    n = rest;
                                }).is_none() { break; }
                            }
                        },
                        _ => (),
                    }
                },
                // Release
                glium::glutin::Event::MouseInput(glium::glutin::ElementState::Released, glium::glutin::MouseButton::Right) if grab.is_some() => {
                    if let Some(Grab { pre_grab_global_env, .. }) = grab {
                        history.add(pre_grab_global_env);
                    }
                    grab = None;
                },
                // Put hovered value on clipboard
                glium::glutin::Event::MouseInput(glium::glutin::ElementState::Released, glium::glutin::MouseButton::Right) => {
                    let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                    do_toplevel(global_env.clone(), |val, x, y| {
                        let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                        if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                            println!("{:?}", path);
                            clipboard = navigate(&val, skip_n(hover_depth as usize, &path_to_directions(&path)));
                        }
                        None
                    });
                },
                // Replace hovered value with clipboard
                glium::glutin::Event::MouseInput(glium::glutin::ElementState::Released, glium::glutin::MouseButton::Left) => {
                    if !keys_held.contains(&VK::LShift) {
                        let mut hit_something = false;
                        let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                        update_global_env!(do_toplevel(global_env.clone(), |val, x, y| {
                            let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                            if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                hit_something = true;
                                Some(navigate_replace(&val, skip_n(hover_depth as usize, &path_to_directions(&path)), clipboard.clone()))
                            } else {
                                Some(val)
                            }
                        }));
                        if !hit_something {
                            let x = world_hover_pos.x as i64;
                            let y = world_hover_pos.y as i64;
                            update_global_env!(::builtins::eval::value_eval(tree!(
                                *("apply_builtin"
                                    (("workspace"
                                      *("quote" &clipboard x y) *("load" "workspace"))
                                     *("quote" &global_env))
                                    @{::builtins::functions::assoc_list_set})
                                *("assoc_list" .) &global_env
                            )).map_err(|e| { println!("Error adding new: {:?}", e); e }).unwrap_or(global_env.clone()));
                        }
                    } else {
                        let ref mut clipboard_ref = clipboard;
                        let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                        update_global_env!(do_toplevel(global_env.clone(), |val, x, y| {
                            let layout = layout_cache.get_layout(val.clone(), global_env.clone());
                            if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                let t = clipboard_ref.clone();
                                let dirs = path_to_directions(&path);
                                let dirs = skip_n(hover_depth as usize, &dirs);
                                *clipboard_ref = navigate(&val, dirs);
                                Some(navigate_replace(&val, dirs, t))
                            } else {
                                Some(val)
                            }
                        }));
                    }
                },
                glium::glutin::Event::MouseInput(glium::glutin::ElementState::Pressed, glium::glutin::MouseButton::Middle) => {
                    let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                    world_panning_anchor = Some(world_hover_pos);
                },
                glium::glutin::Event::MouseInput(glium::glutin::ElementState::Released, glium::glutin::MouseButton::Middle) => {
                    world_panning_anchor = None;
                }
                glium::glutin::Event::MouseWheel(delta, _) => {
                    let change = match delta {
                        glium::glutin::MouseScrollDelta::LineDelta(_x, y) | glium::glutin::MouseScrollDelta::PixelDelta(_x, y) => {
                            if y > 0.0 { -1 } else { 1 }
                        }
                    };
                    // Zoom
                    if keys_held.contains(&VK::LControl) {
                        zoom_power -= change as f32 * 0.2;
                    }
                    // Change the hover depth
                    else {
                        hover_depth = std::cmp::max(hover_depth + change, 0);
                    }
                },
				_ => (),
			}
		}

        if !text_entry {
            let movement = (25.0 * 2.0f32.powf(-zoom_power)).ceil();
            // Scroll
            if keys_held.contains(&VK::Up) || keys_held.contains(&VK::W) { scroll_offset.y += movement; }
            if keys_held.contains(&VK::Down) || keys_held.contains(&VK::S) { scroll_offset.y -= movement; }
            if keys_held.contains(&VK::Left) || keys_held.contains(&VK::A) { scroll_offset.x -= movement; }
            if keys_held.contains(&VK::Right) || keys_held.contains(&VK::D) { scroll_offset.x += movement; }
        }

        top_num += 1;
        {
            if let Some(world_panning_anchor) = world_panning_anchor {
                // Adjust scroll_offset so that world_hover_pos == world_panning_anchor
                let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                scroll_offset = scroll_offset + world_panning_anchor - world_hover_pos;
                scroll_offset.x = scroll_offset.x.round();
                scroll_offset.y = scroll_offset.y.round();
            }
            if let Some(Grab { world_mouse_offset, .. }) = grab {
                let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
                let new_item_loc = world_hover_pos + world_mouse_offset;
                if let Ok(workspace) = builtins::functions::assoc_list_lookup(tree!(&workspace_str &global_env)) {
                    match_tree!(workspace; [[item [_ _]] rest] => {
                        let nx = new_item_loc.x as i64;
                        let ny = new_item_loc.y as i64;
                        let new_workspace = tree!((item nx ny) rest);
                        global_env = ::builtins::functions::assoc_list_set(tree!((&workspace_str new_workspace) &global_env)).unwrap();
                    });
                }
            }

            use render::GeometryGenerator;
            let mut frame = display.draw();
            let (hud_width, hud_height) = { let (w, h) = frame.get_dimensions(); (w as f32, h as f32) };
            let locked_hud_view = orthographic(0.0, hud_width, 0.0, hud_height, -50.0, 50.0);
            window_to_gl = orthographic(0.0, hud_width, hud_height, 0.0, -50.0, 50.0);
            world_to_gl = locked_hud_view * render::scale(2.0f32.powf(zoom_power), 2.0f32.powf(zoom_power), 1.0) * render::translate(-scroll_offset.x, -scroll_offset.y, 0.0);
            frame.clear_color(1.0, 1.0, 1.0, 1.0);
            {
                let mut batch = geometry.batch(&mut frame, &world_to_gl);
                batch.draw_quad(na::Vector2::new(100.0, 200.0), na::Vector2::new(100.0, 10.0), render::color(0xFF00FFFF));
                // batch.draw_string(na::Vector2::new(100.0, 200.0), render::color(0x0000FFFF), "THE WHAT?");
                // batch.draw_string(na::Vector2::new(100.0, 100.0), render::color(0xFF0000FF), "WHAT?");
                // f.layout(Default::default()).draw(na::Vector2::new(400.0, 400.0), &mut batch);
                // examples[3].0 = Rc::new(Value::Number(hover_depth));
            }
            let mut rects_to_highlight = vec![];
            let mut s = None;
            let world_hover_pos = mat_transform_2d(window_to_world(window_to_gl, world_to_gl), window_hover_pos);
            if let Ok(workspace) = builtins::functions::assoc_list_lookup(tree!(&workspace_str &global_env)) {
                for entry in list_to_vec(&workspace).into_iter().rev() {
                    match_tree!(entry; [item [{Value::Number(x)} {Value::Number(y)}]] => {
                        let x = x as f32;
                        let y = y as f32;
                        {
                            let layout = layout_cache.get_layout(item.clone(), global_env.clone());
                            if let Some(path) = layout.hit(na::Vector2::new(x, y), world_hover_pos) {
                                let dirs = path_to_directions(&path);
                                let dirs = skip_n(hover_depth as usize, &dirs);
                                let target = navigate(&item, dirs);
                                rects_to_highlight.clear();
                                layout.get_highlight_rects(na::Vector2::new(x, y), &directions_to_path(dirs), &mut rects_to_highlight);

                                // toplevel.cached_layout.draw(toplevel.position, Some(path), &mut batch);
                                s = s.or(Some(target));
                            }
                        }
                        let geo = layout_cache.get_geometry(item.clone(), context, &mut geometry, global_env.clone());
                        geometry.draw_geometry(&mut frame, &(world_to_gl * render::translate(x, y, 0.0)), &geo);
                    });
                }
            }
            {
                let mut batch = geometry.batch(&mut frame, &world_to_gl);
                for (pos, size) in rects_to_highlight.into_iter().take(1) {
                    batch.draw_quad(pos + size / 2.0, size, render::color(0x00FFFF99));
                }
                // do_layout(&clipboard).draw(na::Vector2::new(0.0, 0.0), &mut batch);
                // batch.draw_quad(hover_pos, na::Vector2::new(5.0, 5.0), render::color(0xFF00FFFF));
            }
            let tooltip = value_tooltip(&s.clone().unwrap_or_else(|| Rc::new(Value::Placeholder)), &global_env);
            match *tooltip {
                Value::Placeholder => (),
                _ => {
                    let geo = layout_cache.get_geometry(tooltip, context, &mut geometry, global_env.clone());
                    geometry.draw_geometry(&mut frame, &(world_to_gl * render::translate(world_hover_pos.x + 20.0, world_hover_pos.y + 20.0, 0.0)), &geo);
                }
            }
            {
                let geo = layout_cache.get_geometry(clipboard.clone(), context, &mut geometry, global_env.clone());
                geometry.draw_geometry(&mut frame, &locked_hud_view, &geo);
            }
            if let Some(hovered) = s {
                if let Some(to_display) = match *hovered {
                    Value::Compound(ref inner) => match **inner {
                        Value::Cons(ref left, _) => Some(left.clone()),
                        _ => None
                    },
                    _ => builtins::functions::type_of(hovered.clone()).ok()
                } {
                    let geo = layout_cache.get_geometry(to_display.clone(), context, &mut geometry, global_env.clone());
                    geometry.draw_geometry(&mut frame, &(world_to_gl * render::translate(world_hover_pos.x + 20.0, world_hover_pos.y - 20.0, 0.0)), &geo);
                }
            }
            frame.finish().unwrap();
        }
        let next_frame = frame_start + 16_000_000;
        let current_time = time::precise_time_ns();
        if current_time < next_frame {
            let time_to_sleep_nanos = next_frame - current_time;
            std::thread::sleep(std::time::Duration::new(time_to_sleep_nanos / 1_000_000_000, (time_to_sleep_nanos % 1_000_000_000) as u32));
        }
	}
}
