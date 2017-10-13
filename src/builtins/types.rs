use std::rc::Rc;
use ::Value;

fn basic_type(s: &str) -> Rc<Value> {
    Rc::new(Value::String(s.to_string()))
}

pub fn quote_type() -> Rc<Value> { basic_type("quote") }
pub fn branch_type() -> Rc<Value> { basic_type("branch") }
pub fn load_type() -> Rc<Value> { basic_type("load") }
pub fn let_type() -> Rc<Value> { basic_type("let") }
pub fn apply_builtin_type() -> Rc<Value> { basic_type("apply_builtin") }
pub fn function_type() -> Rc<Value> { basic_type("function") }
pub fn apply_closure_type() -> Rc<Value> { basic_type("apply_closure") }
pub fn apply_function_type() -> Rc<Value> { basic_type("apply_function") }
pub fn assoc_list_type() -> Rc<Value> { basic_type("assoc_list") }
pub fn closure_type() -> Rc<Value> { basic_type("closure") }
pub fn comment_type() -> Rc<Value> { basic_type("comment") }