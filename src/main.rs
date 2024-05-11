mod parser;
mod ast;
mod lower;
mod error;
mod ir;

use std::fs::read_to_string;

use crate::error::{report_parse_error, report_error};

fn main() {
    let src = read_to_string("test").unwrap();

    let mut ast = parser::all(&src)
        .unwrap_or_else(|e| report_parse_error(
            &src,
            "test",
            e
        ));
    
    dbg!(&ast);

    let mut interp = ir::Interpreter::new();

    lower::lower(&mut ast)
        .resolve_globals()
        .check_global_signatures().unwrap_or_else(|e| report_error(
            &src,
            "test",
            e
        ))
        .check_function_bodies().unwrap_or_else(|e| report_error(
            &src,
            "test",
            e
        ))
        .generate_ir(&mut interp);

    dbg!(&interp);

    dbg!(
        interp.invoke_function("foo", vec![
            ir::Value::Uint(32),
            ir::Value::Uint(91)
        ])
    );
}
