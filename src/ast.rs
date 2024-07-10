use std::fmt::{self, Debug, Formatter};

#[derive(Clone, Copy)]
pub struct Span { pub begin: usize, pub end: usize }

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.begin, self.end)
    }
}

impl Span {
    pub fn new(begin: usize, end: usize) -> Self {
        Self { begin, end }
    }
}

#[derive(Debug)]
pub enum Global {
    Function(Function),
    Class(Class)
}

pub struct Class {
    pub name: String,
    pub fields: Vec<IdentType>
}

impl Debug for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct(&self.name);

        for field in &self.fields {
            s.field(&field.name, &field.ty);
        }

        return s.finish();
    }
}

pub struct Function {
    pub signature: FunctionSignature,
    pub body: Expr
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn {:?} = {:#?}", self.signature, self.body)
    }
}

pub struct FunctionSignature {
    pub span: Span,
    pub name: String,
    pub parameters: Vec<IdentType>,
    pub return_type: Type
}

impl Debug for FunctionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {:?} -> {:?}",
            self.name,
            self.parameters,
            self.return_type
        )
    }
}

#[derive(Clone)]
pub struct IdentType {
    pub span: Span,
    pub name: String,
    pub ty: Type
}

impl Debug for IdentType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?}", self.name, self.ty)
    }
}

pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}

impl Stmt {
    pub fn new(span: Span, kind: StmtKind) -> Self {
        Self { span, kind }
    }
}

pub enum StmtKind {
    Expr(Expr),
    Return(Expr),
    Var(String, Option<Type>, Expr),
    Let(String, Option<Type>, Expr),
    Assign(Expr, Expr)
}

impl Debug for StmtKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(e) => write!(f, "Expr({:#?})", e),
            Self::Return(e) => write!(f, "return {:#?}", e),
            Self::Let(n, t, e) => write!(
                f,
                "let {}: {:?} = {:#?}",
                n,
                t,
                e
            ),
            Self::Var(n, t, e) => write!(
                f,
                "var {}: {:?} = {:#?}",
                n,
                t,
                e
            ),
            Self::Assign(l, r) => write!(
                f,
                "{:#?} = {:#?}",
                l,
                r
            )
        }
    }
}

pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub ty: Option<Type>,
    pub is_mut: bool
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self { span, kind, ty: None, is_mut: false }
    }
}

pub enum ExprKind {
    Unit,
    True,
    False,
    IntLiteral(i32),
    UintLiteral(u32),
    StringLiteral(String),
    ClassConstruct(String, Vec<(String, Expr)>),
    ArrayConstruct(Vec<Expr>),
    GetVar(String),
    FieldAccess(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Arithmetic(ArithmeticOp, Box<Expr>, Box<Expr>),
    Comparison(Comparison, Box<Expr>, Box<Expr>),
    Logical(Logical, Box<Expr>, Box<Expr>),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Try(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>)
}

impl Debug for ExprKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("()"),
            Self::True => f.write_str("true"),
            Self::False => f.write_str("false"),
            Self::IntLiteral(i) => write!(f, "(int){}", i),
            Self::UintLiteral(i) => write!(f, "(uint){}", i),
            Self::StringLiteral(s) => write!(f, "{:?}", s),
            Self::ClassConstruct(class, fields) => {
                let mut builder = f.debug_struct(
                    &format!("new {}", class)
                );

                for (name, value) in fields {
                    builder.field(&name, value);
                }

                builder.finish()
            },
            Self::ArrayConstruct(elems) => write!(f, "Array{:#?}", elems),
            Self::GetVar(n) => write!(f, "GetVar({})", n),
            Self::FieldAccess(e, n) => write!(
                f,
                "{:#?}.{}",
                e,
                n
            ),
            Self::Index(arr, idx) => write!(f, "Index({:#?}, {:#?})", arr, idx),
            Self::Arithmetic(op, l, r) => {
                f.debug_tuple(&format!("Arithmetic<{:?}>", op))
                    .field(&l)
                    .field(&r)
                    .finish()
            },
            Self::Comparison(op, l, r) => {
                f.debug_tuple(&format!("Comparison<{:?}>", op))
                    .field(&l)
                    .field(&r)
                    .finish()
            },
            Self::Logical(op, l, r) => {
                f.debug_tuple(&format!("Logical<{:?}>", op))
                    .field(&l)
                    .field(&r)
                    .finish()
            },
            Self::Block(b, e) => {
                f.write_str("Block ")?;

                let mut builder = f.debug_set();

                for s in b {
                    builder.entry(s);
                }

                if let Some(e) =  e {
                    builder.entry(e);
                }

                builder.finish()
            },
            Self::If(i, t, e) => {
                let mut builder = f.debug_tuple("If");

                builder.field(i);
                builder.field(t);

                if let Some(e) = e {
                    builder.field(e);
                }

                builder.finish()
            },
            Self::Try(e) => write!(f, "{:#?}?", e),
            Self::Call(e, a) => write!(
                f,
                "Call({:#?}){:#?}",
                e,
                a
            )
        }
    }
}

#[derive(Debug)]
pub enum ArithmeticOp { Add, Sub, Mul, Div, Mod }

#[derive(Debug)]
pub enum Comparison { Eq, Ne, Lt, Le, Gt, Ge }

#[derive(Debug)]
pub enum Logical { And, Or }

#[derive(Clone, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Uint,
    String,
    Class(bool, String),
    Option(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Array(bool, Box<Type>),
    Fn(Vec<Type>, Box<Type>)
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("()"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Uint => f.write_str("uint"),
            Self::String => f.write_str("string"),
            Self::Class(m, n) => write!(
                f,
                "{}{}",
                if *m { "mut " } else { "" },
                n
            ),
            Self::Option(t) => write!(
                f,
                "{:?}?",
                t
            ),
            Self::Result(t, e) => write!(
                f,
                "{:?}?{:?}",
                t,
                e
            ),
            Self::Array(m, t) => write!(
                f,
                "{}[{:?}]",
                if *m { "mut " } else { "" },
                t
            ),
            Self::Fn(a, r) => write!(
                f,
                "fn {:?} -> {:?}",
                a,
                r
            )
        }
    }
}
