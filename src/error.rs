use crate::ast::{Span, Type};

use peg::{error::ParseError, str::LineCol};

pub fn report_parse_error(
    file_text: &str,
    file_name: &str,
    err: ParseError<LineCol>
) -> ! {
    eprintln!("Syntax error: expected {}", err.expected);

    eprintln!("--> {}:{}", file_name, err.location);

    let line = file_text.lines().nth(err.location.line - 1).unwrap_or("");

    eprintln!("| {}", line);

    let col = err.location.column;

    print_err_underline(Span::new(col - 1, col), line.len());

    std::process::abort();
}

fn print_err_underline(span: Span, len: usize) {
    eprint!("  ");

    for i in 0..len {
        if i >= span.begin && i < span.end {
            eprint!("^");
        } else {
            eprint!(" ");
        }
    }

    eprintln!();
}

pub fn report_error(file_text: &str, file_name: &str, err: Error) -> ! {
    eprint!("Error: ");

    match err.kind {
        ErrorKind::UndefinedSymbol(sym) => {
            eprintln!("Symbol '{}' isn't defined", sym);
        },
        ErrorKind::TypeMismatch(e, f) => {
            eprintln!("Expected '{:?}', got '{:?}'", e, f);
        },
        ErrorKind::CantInferType => {
            eprintln!("Can't infer the type of this expression");
        },
        ErrorKind::NotAFunction(t) => {
            eprintln!("Type '{:?}' isn't a function type", t);
        },
        ErrorKind::WrongNumberArgs(expected, got) => {
            eprintln!("Expected {} arguments, got {}", expected, got);
        },
        ErrorKind::NotAClass(t) => {
            eprintln!("Type '{:?}' is not a class", t);
        },
        ErrorKind::NotAnArray(t) => {
            eprintln!("Can't index into non-array type '{:?}'", t);
        },
        ErrorKind::FieldNotInClass(f, c) => {
            eprintln!("No field '{}' in class '{}'", f, c);
        },
        ErrorKind::FieldNotInitialized(f) => {
            eprintln!("Field '{}' isn't initialized", f)
        },
        ErrorKind::FieldReInitialized(f) => {
            eprintln!("Can't initialize field '{}' multiple times", f)
        },
        ErrorKind::TypeNotTryable(t) => {
            eprintln!("Can't perform '?' operation on type '{:?}'", t);
        },
        ErrorKind::TypeNotTryableIn(t, r) => {
            eprintln!(
                "Can't perform '?' operation on type '{:?}' in function that returns {:?}",
                t,
                r
            );
        },
        ErrorKind::TypeNotScalar(t) => {
            eprintln!("Can't perform arithmetic operation on type '{:?}'", t);
        },
        ErrorKind::AssignToImmutable => {
            eprintln!("Can't assign to immutable destination");
        }
    }

    let mut line = 1;
    let mut line_offset = 0;

    for (i, c) in file_text.char_indices() {
        if c == '\n' {
            if i <= err.at.begin {
                line += 1;
                line_offset = i + 1;
            } else {
                break;
            }
        }
    }

    let start_col = err.at.begin - line_offset;
    let end_col = err.at.end - line_offset;

    eprintln!("--> {}:{}:{}", file_name, line, start_col);

    let line = file_text.lines().nth(line - 1).unwrap();

    eprintln!("| {}", line);

    print_err_underline(Span::new(start_col, end_col), line.len());

    std::process::abort();
}

#[derive(Debug)]
pub struct Error {
    pub at: Span,
    pub kind: ErrorKind
}

#[derive(Debug)]
pub enum ErrorKind {
    UndefinedSymbol(String),
    TypeMismatch(Type, Type),
    CantInferType,
    NotAFunction(Type),
    WrongNumberArgs(usize, usize),
    NotAClass(Type),
    NotAnArray(Type),
    FieldNotInClass(String, String),
    FieldNotInitialized(String),
    FieldReInitialized(String),
    TypeNotTryable(Type),
    TypeNotTryableIn(Type, Type),
    TypeNotScalar(Type),
    AssignToImmutable
}
