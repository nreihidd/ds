use ::{Value, EvalResult, EvalErr};
use std::rc::Rc;
use serialize::BUILTIN_SERIALIZE_MAPPING;

pub fn tree_layout(arg: Rc<Value>) -> EvalResult {
    let (value, context) = uncons!(arg);
    Ok(match **value {
        Value::Cons(ref left, ref right) => {
            tree!(
                "dbgtree"
                ("navright" {try!(tree_layout(tree!(right .)))} .)
                ("navleft" {try!(tree_layout(tree!(left .)))} .)
                .
            )
        },
        Value::Compound(ref value) => {
            let (ty, expr) = uncons!(value);
            tree!(
                "horizontal"
                ("navcompound" ("navleft" {try!(tree_layout(tree!(ty .)))} .) .)
                ("navcompound" ("navright" {try!(tree_layout(tree!(expr .)))} .) .)
                .
            )
        },
        Value::Nothing => tree!("text" "nothing" [127 127 127 255] .),
        Value::Placeholder => tree!("text" "placeholder" [255 0 0 255] .),
        Value::Number(n) => tree!("text" {format!("{}", n)} [0 0 0 255] .),
        Value::Boolean(b) => tree!("text" {format!("{}", b)} [0 0 255 255] .),
        Value::String(_) => tree!("text" value [255 0 255 255] .),
        Value::Builtin(f) => BUILTIN_SERIALIZE_MAPPING.with(|builtin_mapping| {
            let builtin_mapping = builtin_mapping.borrow();
            let builtin_str = *builtin_mapping.get_a(&f).unwrap_or(&::BUILTIN_MISSING_NAME);
            tree!("text" builtin_str [0 127 0 255] .)
        }),
    })
}