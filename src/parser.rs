use crate::ast::*;

pub use self::global::*;

static RESERVED_WORDS: [&'static str; 13] = [
    "fn",
    "let",
    "mut",
    "return",
    "if",
    "else",
    "while",
    "bool",
    "int",
    "uint",
    "string",
    "true",
    "false"
];

peg::parser! {
    grammar global() for str {
        rule spanned<T>(x: rule<T>) -> (T, Span)
            = b:position!() v:x() e:position!()
            { (
                v,
                Span::new(b, e)
            ) }
        
        rule s0() = quiet! { [' ' | '\t' | '\r' | '\n']* }
        rule s1() = quiet! { [' ' | '\t' | '\r' | '\n']+ }

        pub rule all() -> Vec<Global>
            = s0() d:((
                f:function() { Global::Function(f) }
                / c:class() { Global::Class(c) }
            ) ** s0()) s0() { d }
        
        pub rule class() -> Class
            = "class" s1() name:ident() s0() "{" s0()
                fields: ident_type() ** (s0() "," s0())
            s0() "}" s0() ";" {
                Class { name, fields }
            }

        pub rule function() -> Function
            = "fn" s1() signature:signature() s0() "=" s0() body:expr() s0() ";" {
                Function { signature, body }
            }
        
        rule signature() -> FunctionSignature
            = b:position!() name:ident() s0() "(" s0()
                parameters: ident_type() ** (s0() "," s0())
            s0() ")" s0() "->" s0() return_type:ty() e:position!() { FunctionSignature {
                span: Span::new(b, e),
                name,
                parameters,
                return_type
            } }
        
        rule ident_type() -> IdentType
            = b:position!() name:ident() s0() ":" s0() ty:ty() e:position!() {
                IdentType { span: Span::new(b, e), name, ty }
            }
        
        pub rule stmt() -> Stmt
            = k:spanned(<
                "return" s1() e:expr() {
                    StmtKind::Return(e)
                }
                / "let" s1() n:ident() s0() ":" s0() t:ty() s0() "=" s0() e:expr() {
                    StmtKind::Let(
                        n,
                        t,
                        e
                    )
                }
                / "let" s1() "mut" s1() n:ident() s0() ":" s0() t:ty() s0() "=" s0() e:expr() {
                    StmtKind::LetMut(
                        n,
                        t,
                        e
                    )
                }
                / l:expr() s0() "=" s0() r:expr() {
                    StmtKind::Assign(l, r)
                }
                / e:expr() { StmtKind::Expr(e) }
            >) s0() ";" { Stmt::new(k.1, k.0) }
        
        pub rule ty() -> Type = precedence! {
            "fn" s0() "(" s0()
                a:(ty() ** (s0() "," s0()))
            s0() ")" s0() "->" s0() r:ty() { Type::Fn(a, Box::new(r)) }
            --
            t:(@) s0() "?" e:@ { Type::Result(Box::new(t), Box::new(e)) }
            --
            t:(@) "?" { Type::Option(Box::new(t)) }
            --
            "(" s0() ")" { Type::Unit }
            "bool" { Type::Bool }
            "uint" { Type::Uint }
            "int" { Type::Int }
            "string" { Type::String }
            m:("mut" s1())? n:ident() { Type::Class(m.is_some(), n) }
            m:("mut" s1())? "[" s0() t:ty() s0() "]" {
                Type::Array(m.is_some(), Box::new(t))
            }
        }

        pub rule expr() -> Expr = precedence! {
            x:(@) s0() "||" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Logical(
                    Logical::Or,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            --
            x:(@) s0() "&&" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Logical(
                    Logical::And,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            --
            x:(@) s0() "==" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Comparison(
                    Comparison::Eq,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() "!=" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Comparison(
                    Comparison::Ne,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() "<" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Comparison(
                    Comparison::Lt,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() "<=" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Comparison(
                    Comparison::Le,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() ">" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Comparison(
                    Comparison::Gt,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() ">=" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Comparison(
                    Comparison::Ge,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            --
            x:(@) s0() "+" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Arithmetic(
                    ArithmeticOp::Add,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() "-" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Arithmetic(
                    ArithmeticOp::Sub,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            --
            x:(@) s0() "*" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Arithmetic(
                    ArithmeticOp::Mul,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() "/" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Arithmetic(
                    ArithmeticOp::Div,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            x:(@) s0() "%" s0() y:@ { Expr::new(
                Span::new(x.span.begin, y.span.end),
                ExprKind::Arithmetic(
                    ArithmeticOp::Mod,
                    Box::new(x),
                    Box::new(y)
                )
            ) }
            --
            x:@ s0() "?" e:position!() { Expr::new(
                Span::new(x.span.begin, e),
                ExprKind::Try(Box::new(x))
            ) }
            --
            x:@ s0() "." s0() f:ident() e:position!() { Expr::new(
                Span::new(x.span.begin, e),
                ExprKind::FieldAccess(Box::new(x), f)
            ) }
            --
            x:@ s0() "("
                s0() a:expr() ** s0() s0()
            ")" e:position!() { Expr::new(
                Span::new(x.span.begin, e),
                ExprKind::Call(Box::new(x), a)
            ) }
            --
            t:spanned(<"true">) { Expr::new(
                t.1,
                ExprKind::True
            ) }
            f:spanned(<"false">) { Expr::new(
                f.1,
                ExprKind::False
            ) }
            b:position!() "new" s1() c:ident() s0() "{" s0()
                f:((n:ident() s0() ":" s0() v:expr() { (n, v) }) ** (s0() "," s0()))
            s0() "}" e:position!() { Expr::new(
                Span::new(b, e),
                ExprKind::ClassConstruct(c, f)
            ) }
            v:spanned(<ident()>) { Expr::new(
                v.1,
                ExprKind::GetVar(v.0)
            ) }
            b:position!() "if" s0() i:expr() s0() "then" s0() t:expr()
                f:(s0() "else" s0() f:expr() { f })? e:position!() { Expr::new(
                    Span::new(b, e),
                    ExprKind::If(
                        Box::new(i),
                        Box::new(t),
                        f.map(Box::new)
                    )
                ) }
            b:position!() "{"
                s0() s:stmt() ** s0() s0()
                x:expr()? s0()
            "}" e:position!() { Expr::new(
                Span::new(b, e),
                ExprKind::Block(s, x.map(Box::new))
            ) }
            "(" s0() x:expr() s0() ")" { x }
            l:spanned(<uint_literal()>) { Expr::new(
                l.1,
                ExprKind::UintLiteral(l.0)
            ) }
            l:spanned(<int_literal()>) { Expr::new(
                l.1,
                ExprKind::IntLiteral(l.0)
            ) }
            s:spanned(<"(" s0() ")">) { Expr::new(
                s.1,
                ExprKind::Unit
            ) }
        }

        rule int_literal() -> i32
            = n:$("-"? ['0'..='9']+) {? n.parse().or(Err("int")) }

        rule uint_literal() -> u32
            = n:$(['0'..='9']+) "u" {? n.parse().or(Err("uint")) }
        
        rule ident() -> String
            = i:$(
                ['A'..='Z' | 'a'..='z' | '_']
                ['A'..='Z' | 'a'..='z' | '0'..='9' | '_']*
            ) {?
                if RESERVED_WORDS.contains(&i) { Err("ident") }
                else { Ok(i.into()) }
            }
    }
}
