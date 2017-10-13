use ::{Value, EvalResult, error_to_value};
use std::rc::Rc;
use builtins::functions::*;
use builtins::eval::value_eval;

pub fn test_evals(global_env: &Rc<Value>) -> (Rc<Value>, Rc<Value>, Rc<Value>) {
    fn make_symbol(name: &str) -> Rc<Value> {
        tree!(name)
    }

    let s1337 = &make_symbol("1337");

    let mut results = tree!(.);
    let basic_env = tree!(*("assoc_list" .) global_env);
    macro_rules! test { ($e:expr) => {{
        let v: EvalResult = $e;
        println!("{:?}", v);
        results = tree!({v.unwrap_or(tree!(!))} results);
    }} };
    macro_rules! test_eval { ($e:expr) => {{
        let s: &Rc<Value> = &$e;
        let v: EvalResult = value_eval(tree!(s &basic_env));
        println!("{:?} --> {:?}", s, v);
        let to_append = v.unwrap_or_else(error_to_value);
        results = tree!(s to_append results);
    }} };
    test!(value_eval(tree!(20 &basic_env)));
    let c = tree!(*("quote" *("quote" 10)));
    test_eval!(c);
    let e = tree!(*("apply_builtin" (5 10) @add));
    test_eval!(e);
    test_eval!(tree!(*("load" s1337)));
    test_eval!(tree!(20 5));
    test!(Ok(tree!(!)));
    let myarg = &make_symbol("alpha");
    let f = tree!(
        *("function" myarg
            *("apply_builtin" (1 *("load" myarg)) @add))
    );

    test!(Ok(f.clone()));
    test_eval!(tree!(*("apply_closure" 5 f)));

    test_eval!(Rc::new(Value::Placeholder));

    let s3 = &make_symbol("s3");

    let test_let = tree!(
        *("let" (22 s3)
            *("apply_builtin" (5 *("load" s3)) @add))
    );
    test_eval!(test_let);

    test_eval!(tree!(
        *("branch" true "IT'S TRUEEEEE" "FALSE")
    ));

    let s101 = &make_symbol("s101");
    let s102 = &make_symbol("s102");
    let myarg = &make_symbol("MyArg");

    let test_factorial = tree!(
        *("function" myarg
            *("let" (*("apply_builtin" *("load" myarg) @left) s101)
            *("let" (*("apply_builtin" *("load" myarg) @right) s102)
                *("branch"
                    *("apply_builtin" (*("load" s101) 1) @greater_than)
                    *("apply_builtin"
                      (*("load" s101)
                       *("apply_closure"
                         (*("apply_builtin" (*("load" s101) {-1}) @add)
                          *("load" s102))
                         *("load" s102)))
                      @multiply)
                    1))))
    );
    test_eval!(test_factorial.clone());

    test_eval!(tree!(
        *("apply_closure" (5 &test_factorial) test_factorial)
    ));

    let s1 = &make_symbol("s1");
    let s2 = &make_symbol("s2");
    let myarg = &make_symbol("MyArg");

    let test_y = tree!(
        *("function" s1
            *("let"
                (*("function" s2
                    *("function" myarg
                        *("apply_closure"
                            *("load" myarg)
                            *("apply_closure" *("apply_closure" *("load" s2) *("load" s2)) *("load" s1)))))
                 s3)
                *("apply_closure" *("apply_closure" *("load" s3) *("load" s3)) *("load" s1))))
    );

    let myarg = &make_symbol("MyArg");

    let test_y_fact = tree!(
        *("apply_closure"
            *("function" s1
                *("function" myarg
                        *("branch"
                            *("apply_builtin" (*("load" myarg) 1) @greater_than)
                            *("apply_builtin"
                                (*("load" myarg)
                                 *("apply_closure"
                                     *("apply_builtin" (*("load" myarg) {-1}) @add)
                                     *("load" s1)))
                                @multiply)
                            1)
                        ))
            test_y)
    );

    let look_closure = value_eval(tree!(&test_y_fact &basic_env)).unwrap();

    test_eval!(tree!(*("apply_closure" 6 test_y_fact)));

    test_eval!(tree!(*("comment" "A comment" 5)));

    (results, basic_env, look_closure)
}