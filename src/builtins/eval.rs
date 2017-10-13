use ::{Value, EvalResult, EvalErr};
use std::rc::Rc;
use builtins::functions::{assoc_list_lookup, assoc_list_set_shadow};

enum Operation {
	Eval,
	Bind,
	RestoreLocal(Rc<Value>),
	RestoreGlobal(Rc<Value>),
	ApplyBuiltin,
    ApplyClosure,
    ApplyFunction,
	Branch,
	Load,
	Cons,
    AssocList,
	Literal(Rc<Value>),
}

fn wrong_shape(name: &str, val: Rc<Value>) -> EvalErr {
    let mut s = name.to_string();
    s.push_str(" has the wrong shape");
    EvalErr::Detailed(tree!(s val))
}

pub fn value_eval(val_env: Rc<Value>) -> EvalResult {
    use self::Operation::*;
    let mut values: Vec<Rc<Value>> = vec![];
    let mut operations: Vec<Operation> = vec![];

    let (arg, mut local, mut global) = try!(match_tree!(val_env; [arg [local global]] => (arg.clone(), local.clone(), global.clone())).ok_or_else(|| wrong_shape("val_env", val_env)));

    macro_rules! push_ops {
        ($($e: expr),+) => {{
            $(operations.push($e));+
        }}
    }

    push_ops![Eval, Literal(arg)];

    // Can define a max size for operationStack, return Error when it's exceeded
    // Can also reserve that size, maybe

    // value_eval_catch will still recurse, unless if that's handled similarly to value_eval and special-cased here,
    // would have Operation::Catch(value_stack_size) that, upon an error, find the last Catch, pop off values until it's back
    // at value_stack_size length, then push the result to the value stack.  If there was no error, then when Catch is matched, just do nothing and let the value
    // pass on through (or wrap it in some success indicator like [true val], and put the error in [false error])

    macro_rules! push_local {() => {
        if let Some(&RestoreLocal(_)) = operations.iter().rev().skip_while(|op| match **op { Operation::RestoreGlobal(_) => true, _ => false }).next() {
            // Do nothing
        } else {
            operations.push(RestoreLocal(local.clone()));
        }
    }}
    macro_rules! push_global {() => {
        if let Some(&RestoreGlobal(_)) = operations.iter().rev().skip_while(|op| match **op { Operation::RestoreLocal(_) => true, _ => false }).next() {
            // Do nothing
        } else {
            operations.push(RestoreGlobal(global.clone()));
        }
    }}

    fn not_enough_args(s: &str) -> EvalErr {
        EvalErr::Detailed(tree!("not enough values for" s))
    }

    while let Some(op) = operations.pop() {
        match op {
            Literal(val) => {
                values.push(val);
            },
            Bind => {
                let val_id = try!(values.pop().ok_or_else(|| not_enough_args("Bind")));
                let (val, id) = uncons!(val_id);
                local = try!(assoc_list_set_shadow(Value::cons(Value::cons(id.clone(), val.clone()), local)));
            },
            RestoreLocal(l) => {
                local = l;
            },
            RestoreGlobal(g) => {
                global = g;
            },
            ApplyBuiltin => {
                let v = try!(values.pop().ok_or_else(|| not_enough_args("ApplyBuiltin")));
                let (arg, builtin) = try!(match_tree!(v; [arg builtin] => (arg.clone(), builtin.clone())).ok_or_else(|| wrong_shape("apply_builtin", v.clone())));
                if let Value::Builtin(b) = *builtin {
                    // prevent recursion
                    if b == value_eval {
                        let (val, l, g) = try!(match_tree!(arg; [val [l g]] => (val.clone(), l.clone(), g.clone())).ok_or_else(|| wrong_shape("apply_builtin", v)));
                        push_local!();
                        push_global!();
                        local = l;
                        global = g;
                        push_ops![Eval, Literal(val)];
                    } else {
                        values.push(try!(b(arg)));
                    }
                } else {
                    return Err(EvalErr::Type("apply_builtin needs a builtin".to_string()));
                }
            },
            ApplyClosure => {
                let val = try!(values.pop().ok_or_else(|| not_enough_args("ApplyClosure")));
                try!(match_tree!(val; [arg ([_ [env ([_ [arg_id body]])]])] => {
                    push_local!();
                    push_ops![Eval, Literal(body.clone()), Bind, RestoreLocal(env.clone()), Literal(tree!(arg arg_id))];
                }).ok_or_else(|| wrong_shape("ApplyClosure", val.clone())));
            },
            ApplyFunction => {
                let val = try!(values.pop().ok_or_else(|| not_enough_args("ApplyFunction")));
                try!(match_tree!(val; [arg ([_ [arg_id body]])] => {
                    push_local!();
                    push_ops![Eval, Literal(body.clone()), Bind, Literal(tree!(arg arg_id))];
                }).ok_or_else(|| wrong_shape("apply_function", val.clone())));
            },
            Branch => {
                let test = try!(values.pop().ok_or_else(|| not_enough_args("Branch test")));
                let arms = try!(values.pop().ok_or_else(|| not_enough_args("Branch arms")));
                let (if_true, if_false) = uncons!(arms);
                match *test {
                    Value::Boolean(true) => push_ops![Eval, Literal(if_true.clone())],
                    Value::Boolean(false) => push_ops![Eval, Literal(if_false.clone())],
                    _ => return Err(EvalErr::Detailed(tree!("branch tests only booleans, not:" test)))
                }
            },
            Load => {
                let symbol = try!(values.pop().ok_or_else(|| not_enough_args("Load")));
                let value = try!(assoc_list_lookup(Value::cons(symbol.clone(), local.clone())).or_else(|_| {
                    assoc_list_lookup(Value::cons(symbol.clone(), global.clone()))
                }));
                values.push(value);
            },
            Cons => {
                let right = try!(values.pop().ok_or_else(|| not_enough_args("Cons right")));
                let left = try!(values.pop().ok_or_else(|| not_enough_args("Cons left")));
                values.push(Value::cons(left, right));
            },
            AssocList => {
                let inner = try!(values.pop().ok_or_else(|| not_enough_args("AssocList")));
                values.push(tree!(*("assoc_list" inner)));
            },
            Eval => {
                let eval_val = try!(values.pop().ok_or_else(|| not_enough_args("Eval")));
                match *eval_val {
                    Value::Cons(ref left, ref right) => push_ops![Cons, Eval, Literal(right.clone()), Eval, Literal(left.clone())],
                    Value::Compound(ref inner) => {
                        let (ty, val) = uncons!(inner);
                        let mut was_special = false;
                        if let Value::String(ref s) = **ty {
                            was_special = true;
                            match s.as_ref() {
                                "load" => {
                                    push_ops![Load, Eval, Literal(val.clone())];
                                },
                                "function" => {
                                    values.push(tree!(*("closure" &local *("function" val))));
                                },
                                "quote" => {
                                    values.push(val.clone());
                                },
                                "branch" => {
                                    try!(match_tree!(*val; [test if_true_if_false] => {
                                        push_ops![Branch, Eval, Literal(test.clone()), Literal(if_true_if_false.clone())];
                                    }).ok_or_else(|| wrong_shape("branch", val.clone())));
                                },
                                "let" => {
                                    try!(match_tree!(*val; [[val id] body] => {
                                        push_local!();
                                        push_ops![Eval, Literal(body.clone()), Bind, Eval, Literal(Value::cons(val.clone(), id.clone()))];
                                    }).ok_or_else(|| wrong_shape("let", val.clone())));
                                },
                                "apply_function" => {
                                    push_ops![ApplyFunction, Eval, Literal(val.clone())]
                                },
                                "apply_closure" => {
                                    push_ops![ApplyClosure, Eval, Literal(val.clone())]
                                },
                                "apply_builtin" => {
                                    push_ops![ApplyBuiltin, Eval, Literal(val.clone())]
                                },
                                "assoc_list" => {
                                    push_ops![AssocList, Eval, Literal(val.clone())]
                                },
                                _ => was_special = false,
                            }
                        }
                        if !was_special {
                            let eval_dict = try!(assoc_list_lookup(tree!("eval_dict" &global)));
                            match assoc_list_lookup(Value::cons(ty.clone(), eval_dict)) {
                                Ok(eval) => {
                                    let arg_env = Value::cons(val.clone(), Value::cons(local.clone(), global.clone()));
                                    match *eval {
                                        Value::Builtin(b) => {
                                            push_ops![ApplyBuiltin, Literal(Value::cons(arg_env, eval.clone()))]
                                        },
                                        _ => {
                                            push_ops![Eval, Literal(tree!(*("apply_closure" *("quote" arg_env) *("quote" eval))))];
                                        }
                                    }
                                },
                                _ => {
                                    println!("WARNING: Eval'ing to self: {:?}", ty);
                                    values.push(eval_val.clone());
                                }
                            }
                        }
                    },
                    Value::Placeholder => return Err(EvalErr::Placeholder),
                    _ => values.push(eval_val.clone()),
                }
            },
        }
    }

    values.pop().ok_or_else(|| EvalErr::Detailed(tree!("Something went terribly wrong")))
}