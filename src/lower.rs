use crate::{
    ast::*,
    error::*, ir
};

use std::collections::HashMap;

pub struct Lower<'a, Stage> {
    stage: Stage,
    ast: Option<&'a mut [Global]>,
    globals: HashMap<String, (Type, u32, Span)>,
    classes: HashMap<String, Vec<IdentType>>
}

pub struct ResolveGlobals {
    num_globals: u32
}

pub struct CheckGlobalSignatures;

pub struct CheckFunctionBodies {
    var_stack: Vec<(IdentType, bool)>,
    locals_in_frame: usize,
    return_type: Type
}

pub struct GenerateIr {
    var_stack: Vec<IdentType>,
    locals_in_frame: usize
}

pub fn lower<'a>(ast: &'a mut [Global]) -> Lower<ResolveGlobals> {
    Lower {
        stage: ResolveGlobals { num_globals: 0 },
        ast: Some(ast),
        globals: HashMap::new(),
        classes: HashMap::new()
    }
}

impl<'a> Lower<'a, ResolveGlobals> {
    pub fn resolve_globals(mut self) -> Lower<'a, CheckGlobalSignatures> {
        let ast = self.ast.take().unwrap();

        for decl in &*ast {
            match decl {
                Global::Class(c) => {
                    self.classes.insert(c.name.clone(), c.fields.clone());
                },
                Global::Function(f) => {
                    self.globals.insert(
                        f.signature.name.clone(),
                        (
                            Type::Fn(f.signature.parameters.iter()
                                .map(|p| p.ty.clone()).collect(),
                                Box::new(f.signature.return_type.clone())
                            ),
                            self.stage.num_globals,
                            f.signature.span
                        )
                    );

                    self.stage.num_globals += 1;
                }
            }
        }

        return Lower {
            stage: CheckGlobalSignatures,
            ast: Some(ast),
            globals: self.globals,
            classes: self.classes
        }
    }
}

impl<'a> Lower<'a, CheckGlobalSignatures> {
    pub fn check_global_signatures(mut self) -> Result<Lower<'a, CheckFunctionBodies>, Error> {
        let ast = self.ast.take().unwrap();

        for decl in &*ast {
            match decl {
                Global::Class(c) => {
                    for field in &c.fields {
                        self.verify_type(field.span, &field.ty)?;
                    }
                },
                Global::Function(f) => {
                    for param in &f.signature.parameters {
                        self.verify_type(param.span, &param.ty)?;
                    }

                    self.verify_type(f.signature.span, &f.signature.return_type)?;
                }
            }
        }

        return Ok(Lower {
            stage: CheckFunctionBodies {
                var_stack: Vec::new(),
                locals_in_frame: 0,
                return_type: Type::Unit
            },
            ast: Some(ast),
            globals: self.globals,
            classes: self.classes
        })
    }

    fn verify_type(&self, at: Span, ty: &Type) -> Result<(), Error> {
        match ty {
            Type::Class(_, name) => if self.classes.contains_key(name) {
                Ok(())
            } else {
                Err(Error {
                    at,
                    kind: ErrorKind::UndefinedSymbol(name.clone())
                })
            },
            Type::Option(t) => self.verify_type(at, t),
            Type::Result(t, e) => self.verify_type(at, t)
                .and_then(|_| self.verify_type(at, e)),
            Type::Array(_, t) => self.verify_type(at, t),
            Type::Fn(p, r) => {
                for t in p {
                    self.verify_type(at, t)?;
                }

                self.verify_type(at, r)?;

                Ok(())
            },
            _ => Ok(())
        }
    }
}

impl<'a> Lower<'a, CheckFunctionBodies> {
    pub fn check_function_bodies(mut self) -> Result<Lower<'a, GenerateIr>, Error> {
        let ast = self.ast.take().unwrap();

        for d in &mut *ast {
            if let Global::Function(f) = d {
                for param in &f.signature.parameters {
                    self.stage.var_stack.push((param.clone(), false));
                }

                self.assert_types_match(
                    &f.signature.return_type,
                    &mut f.body
                )?;

                self.stage.var_stack.clear();
            }
        }

        return Ok(Lower {
            stage: GenerateIr {
                var_stack: Vec::new(),
                locals_in_frame: 0
            },
            ast: Some(ast),
            globals: self.globals,
            classes: self.classes
        });
    }

    fn assert_types_match(
        &mut self,
        ty: &Type,
        ex: &mut Expr
    ) -> Result<(), Error> {
        if ex.ty.is_none() {
            self.resolve_type(ex)?;
        }

        let found = ex.ty.as_ref().unwrap();

        return if ty == found || self.can_coerce(found, ty) {
            Ok(())
        } else {
            Err(Error {
                at: ex.span,
                kind: ErrorKind::TypeMismatch(ty.clone(), found.clone())
            })
        };
    }

    fn resolve_type(
        &mut self,
        ex: &mut Expr
    ) -> Result<(), Error> {
        ex.ty = Some(match &mut ex.kind {
            ExprKind::Unit => Type::Unit,
            ExprKind::True => Type::Bool,
            ExprKind::False => Type::Bool,
            ExprKind::IntLiteral(_) => Type::Int,
            ExprKind::UintLiteral(_) => Type::Uint,
            ExprKind::ClassConstruct(class, fields) => {
                let mut fields_init: Vec<_> = self.classes.get(class)
                    .ok_or_else(|| Error {
                        at: ex.span,
                        kind: ErrorKind::UndefinedSymbol(class.clone())
                    })?
                    .iter()
                    .map(|it| (it.clone(), false))
                    .collect();

                for (name, value) in fields.iter_mut() {
                    let i = fields_init.iter()
                        .enumerate()
                        .find(|(_, f)| &f.0.name == name)
                        .map(|(i, _)| i);

                    match i {
                        Some(i) => {
                            if fields_init[i].1 {
                                return Err(Error {
                                    at: ex.span,
                                    kind: ErrorKind::FieldReInitialized(name.clone())
                                });
                            } else {
                                fields_init[i].1 = true;
                            }

                            self.assert_types_match(
                                &fields_init[i].0.ty,
                                value
                            )?;
                        },
                        None => {
                            return Err(Error {
                                at: ex.span,
                                kind: ErrorKind::FieldNotInClass(name.clone(), class.clone())
                            });
                        }
                    }
                }

                for (field, init) in fields_init {
                    if !init {
                        return Err(Error {
                            at: ex.span,
                            kind: ErrorKind::FieldNotInitialized(field.name.clone())
                        });
                    }
                }

                Type::Class(true, class.clone())
            },
            ExprKind::GetVar(var) =>
                self.stage.var_stack.iter().rev()
                    .find_map(|(it, m)| if &it.name == var {
                        ex.is_mut = *m;
                        Some(it.ty.clone())
                    } else { None })
                    .or_else(|| self.globals.get(var)
                        .map(|it| it.0.clone()))
                    .ok_or_else(|| Error {
                        at: ex.span,
                        kind: ErrorKind::UndefinedSymbol(var.clone())
                    })?,
            ExprKind::FieldAccess(l, f) => {
                if l.ty.is_none() {
                    self.resolve_type(&mut *l)?;
                }

                let ty = l.ty.as_ref().unwrap();

                let Type::Class(m, class) = ty else {
                    return Err(Error {
                        at: ex.span,
                        kind: ErrorKind::NotAClass(ty.clone())
                    })
                };
                
                ex.is_mut = *m;

                let Some(fields) = self.classes.get(class) else {
                    return Err(Error {
                        at: l.span,
                        kind: ErrorKind::UndefinedSymbol(class.clone())
                    });
                };

                fields.iter()
                    .find_map(|it| if &it.name == f {
                        Some(it.ty.clone())
                    } else { None })
                    .ok_or_else(|| Error {
                        at: ex.span,
                        kind: ErrorKind::FieldNotInClass(f.clone(), class.clone())
                    })?
            },
            ExprKind::Try(e) => {
                if e.ty.is_none() {
                    self.resolve_type(&mut *e)?;
                }
                
                let t = match e.ty.as_ref().unwrap() {
                    Type::Option(t) => {
                        match self.stage.return_type {
                            Type::Option(_) => (),
                            _ => return Err(Error {
                                at: e.span,
                                kind: ErrorKind::TypeNotTryableIn(
                                    Type::Option(t.clone()),
                                    self.stage.return_type.clone()
                                )
                            })
                        }

                        &*t
                    },
                    Type::Result(t, err) => {
                        match &self.stage.return_type {
                            Type::Result(_, re) if re == err => (),
                            _ => return Err(Error {
                                at: e.span,
                                kind: ErrorKind::TypeNotTryableIn(
                                    Type::Result(t.clone(), err.clone()),
                                    self.stage.return_type.clone()
                                )
                            })
                        }

                        &*t
                    },
                    _ => return Err(Error {
                        at: e.span,
                        kind: ErrorKind::TypeNotTryable(e.ty.clone().unwrap())
                    })
                };

                t.as_ref().clone()
            },
            ExprKind::Arithmetic(_, l, r) => {
                if l.ty.is_none() {
                    self.resolve_type(&mut *l)?;
                }
                if r.ty.is_none() {
                    self.resolve_type(&mut *r)?;
                }

                let lt = l.ty.as_ref().unwrap();
                let rt = r.ty.as_ref().unwrap();

                match lt {
                    Type::Int | Type::Uint => (),
                    _ => return Err(Error {
                        at: l.span,
                        kind: ErrorKind::TypeNotScalar(lt.clone())
                    })
                }

                if lt != rt {
                    return Err(Error {
                        at: r.span,
                        kind: ErrorKind::TypeMismatch(lt.clone(), rt.clone())
                    });
                }

                lt.clone()
            },
            ExprKind::Comparison(_, l, r) => {
                if l.ty.is_none() {
                    self.resolve_type(&mut *l)?;
                }

                let lt = l.ty.as_ref().unwrap();

                match lt {
                    Type::Int | Type::Uint => (),
                    _ => return Err(Error {
                        at: l.span,
                        kind: ErrorKind::TypeNotScalar(lt.clone())
                    })
                }

                self.assert_types_match(lt, r)?;

                Type::Bool
            },
            ExprKind::Logical(_, l, r) => {
                if l.ty.is_none() {
                    self.resolve_type(&mut *l)?;
                }

                let lt = l.ty.as_ref().unwrap();

                if lt != &Type::Bool {
                    return Err(Error {
                        at: l.span,
                        kind: ErrorKind::TypeMismatch(Type::Bool, lt.clone())
                    })
                }

                self.assert_types_match(lt, r)?;

                Type::Bool
            },
            ExprKind::If(c, t, e) => {
                self.assert_types_match(&Type::Bool, c)?;
                
                if t.ty.is_none() {
                    self.resolve_type(&mut *t)?;
                }

                let tt = t.ty.as_ref().unwrap();

                if let Some(e) = e {
                    self.assert_types_match(tt, &mut *e)?;
                }

                tt.clone()
            },
            ExprKind::Block(body, tail) => {
                let n = self.stage.locals_in_frame;
                self.stage.locals_in_frame = 0;

                for st in body {
                    self.check_stmt(st)?;
                }

                let ty = match tail {
                    Some(e) => {
                        if e.ty.is_none() {
                            self.resolve_type(&mut *e)?;
                        }

                        e.ty.clone().unwrap()
                    },
                    None => Type::Unit
                };

                for _ in 0..self.stage.locals_in_frame {
                    self.stage.var_stack.pop().unwrap();
                }

                self.stage.locals_in_frame = n;

                ty
            },
            ExprKind::Call(callee, args) => {
                if callee.ty.is_none() {
                    self.resolve_type(&mut *callee)?;
                }

                let Type::Fn(
                    params,
                    ret
                ) = callee.ty.as_ref().unwrap() else {
                    return Err(Error {
                        at: ex.span,
                        kind: ErrorKind::NotAFunction(callee.ty.clone().unwrap())
                    });
                };

                if args.len() != params.len() {
                    return Err(Error {
                        at: ex.span,
                        kind: ErrorKind::WrongNumberArgs(params.len(), args.len())
                    });
                }

                for i in 0..args.len() {
                    let arg = &mut args[i];

                    let param_ty = &params[i];

                    self.assert_types_match(param_ty, arg)?;
                }

                ret.as_ref().clone()
            }
        });

        return Ok(());
    }

    fn can_coerce(&self, from: &Type, to: &Type) -> bool {
        match from {
            Type::Class(true, c0) => match to {
                Type::Class(false, c1) if c1 == c0 => true,
                _ => false
            },
            Type::Array(true, t0) => match to {
                Type::Array(false, t1) if t1 == t0 => true,
                _ => false
            },
            _ => false
        }
    }

    fn check_stmt(
        &mut self,
        st: &mut Stmt
    ) -> Result<(), Error> {
        match &mut st.kind {
            StmtKind::Return(e) => {
                self.assert_types_match(
                    &self.stage.return_type.clone(),
                    e
                )?;
            },
            StmtKind::Expr(e) => {
                if e.ty.is_none() {
                    self.resolve_type(e)?;
                }
            },
            StmtKind::Let(i, t, e) => {
                self.assert_types_match(&*t, e)?;

                self.stage.var_stack.push((IdentType {
                    span: st.span,
                    name: i.clone(),
                    ty: t.clone()
                }, false));

                self.stage.locals_in_frame += 1;
            },
            StmtKind::LetMut(i, t, e) => {
                self.assert_types_match(&*t, e)?;

                self.stage.var_stack.push((IdentType {
                    span: st.span,
                    name: i.clone(),
                    ty: t.clone()
                }, true));

                self.stage.locals_in_frame += 1;
            },
            StmtKind::Assign(l, r) => {
                if l.ty.is_none() {
                    self.resolve_type(l)?;
                }

                if !l.is_mut {
                    return Err(Error {
                        at: st.span,
                        kind: ErrorKind::AssignToImmutable
                    });
                }
            
                let lt = l.ty.as_ref().unwrap();

                self.assert_types_match(lt, r)?;
            }
        }

        return Ok(());
    }
}

impl<'a> Lower<'a, GenerateIr> {
    pub fn generate_ir(mut self, vm: &mut ir::Interpreter) {
        let ast = self.ast.take().unwrap();

        let ns_id = vm.add_namespace(self.globals.len());

        for global in ast {
            if let Global::Function(ast_func) = global {
                let mut ir_func = self.generate_func(ast_func);
                ir_func.globals_namespace = ns_id;

                let func_id = vm.add_function(ir_func);

                let global_id = self.globals
                    .get(&ast_func.signature.name).unwrap().1;

                vm.set_global(ns_id, global_id, ir::Value::Function(func_id));
            }
        }
    }

    fn generate_func(&mut self, ast: &mut Function) -> ir::Function {
        let mut ir = ir::Function {
            name: ast.signature.name.clone(),
            globals_namespace: 0,
            params: 0,
            code: Vec::new()
        };
        self.stage.locals_in_frame = ast.signature.parameters.len();
        ir.params = self.stage.locals_in_frame as u32;

        for param in &ast.signature.parameters {
            self.stage.var_stack.push(param.clone());
        }

        self.lower_expr(&ast.body, &mut ir.code);

        ir.code.push(ir::Op::Ret);

        return ir;
    }

    fn lower_expr(&mut self, expr: &Expr, to: &mut Vec<ir::Op>) -> Option<Place> {
        match &expr.kind {
            ExprKind::Unit => {
                to.push(ir::Op::ConstUnit);
                None
            },
            ExprKind::True => {
                to.push(ir::Op::ConstTrue);
                None
            },
            ExprKind::False => {
                to.push(ir::Op::ConstFalse);
                None
            },
            ExprKind::IntLiteral(i) => {
                to.push(ir::Op::ConstInt(*i));
                None
            },
            ExprKind::UintLiteral(i) => {
                to.push(ir::Op::ConstUint(*i));
                None
            },
            ExprKind::Arithmetic(op, l, r) => {
                self.lower_and_load(l, to);
                self.lower_and_load(r, to);

                match op {
                    ArithmeticOp::Add => to.push(ir::Op::Add),
                    ArithmeticOp::Sub => to.push(ir::Op::Sub),
                    ArithmeticOp::Mul => to.push(ir::Op::Mul),
                    ArithmeticOp::Div => to.push(ir::Op::Div),
                    ArithmeticOp::Mod => to.push(ir::Op::Mod)
                };

                None
            },
            ExprKind::Logical(op, l, r) => {
                self.lower_and_load(l, to);
                self.lower_and_load(r, to);

                match op {
                    Logical::And => to.push(ir::Op::And),
                    Logical::Or => to.push(ir::Op::Or)
                };

                None
            },
            ExprKind::Comparison(op, l, r) => {
                self.lower_and_load(l, to);
                self.lower_and_load(r, to);

                match op {
                    Comparison::Eq => to.push(ir::Op::Eq),
                    Comparison::Ne => to.push(ir::Op::Ne),
                    Comparison::Gt => to.push(ir::Op::Gt),
                    Comparison::Ge => to.push(ir::Op::Ge),
                    Comparison::Lt => to.push(ir::Op::Lt),
                    Comparison::Le => to.push(ir::Op::Le)
                };

                None
            },
            ExprKind::GetVar(name) => {
                let idx = self.stage.var_stack.iter()
                    .rev().enumerate()
                    .find(|(_, var)| &var.name == name)
                    .map(|(i, _)| i as u32);
                
                if let Some(idx) = idx {
                    Some(Place::Local(idx))
                } else {
                    let idx = self.globals.iter()
                        .find(|(g_name, _)| *g_name == name)
                        .map(|(_, (_, i, _))| *i)
                        .unwrap();

                    Some(Place::Global(idx))
                }
            },
            ExprKind::FieldAccess(obj, field) => {
                self.lower_and_load(obj, to);

                let Some(Type::Class(_, class)) = &obj.ty else {
                    unreachable!();
                };

                let idx = self.classes.get(class).unwrap()
                    .iter().enumerate()
                    .find(|(_, it)| &it.name == field)
                    .unwrap().0 as u32;
                
                Some(Place::Field(idx))
            },
            ExprKind::If(cond, t, e) => {
                self.lower_and_load(cond, to);

                let addr_of_jmp = to.len();
                to.push(ir::Op::JumpIfFalse(0)); // stub

                self.lower_and_load(t, to);

                if let Some(e) = e {
                    to[addr_of_jmp] = ir::Op::JumpIfFalse(
                        to.len() as i32 - addr_of_jmp as i32 + 1
                    );

                    let addr_of_jmp = to.len();
                    to.push(ir::Op::Jump(0)); // stub

                    self.lower_and_load(e, to);

                    to[addr_of_jmp] = ir::Op::Jump(
                        to.len() as i32 - addr_of_jmp as i32
                    );
                } else {
                    to[addr_of_jmp] = ir::Op::JumpIfFalse(
                        to.len() as i32 - addr_of_jmp as i32
                    );
                }

                None
            },
            ExprKind::Try(e) => {
                self.lower_and_load(e, to);

                to.push(ir::Op::Try);

                None
            },
            ExprKind::ClassConstruct(c, f) => {
                let class = self.classes.get(c).cloned().unwrap();

                for field in &class {
                    let e = f.iter()
                        .find(|(k, _)| k == &field.name)
                        .map(|(_, v)| v).unwrap();

                    self.lower_and_load(e, to);
                }

                to.push(ir::Op::ConstructObject(class.len() as u32));

                None
            },
            ExprKind::Block(body, tail) => {
                let tmp = self.stage.locals_in_frame;
                self.stage.locals_in_frame = 0;

                for stmt in body {
                    self.lower_stmt(stmt, to);
                }

                if let Some(tail) = tail {
                    self.lower_and_load(tail, to);
                } else {
                    to.push(ir::Op::ConstUnit);
                }

                if self.stage.locals_in_frame > 0 {
                    to.push(ir::Op::DestroyLocals(
                        self.stage.locals_in_frame as u32
                    ));
                }

                self.stage.locals_in_frame = tmp;

                None
            },
            ExprKind::Call(callee, args) => {
                for arg in args {
                    self.lower_and_load(arg, to);
                }

                self.lower_and_load(&callee, to);

                to.push(ir::Op::Call);

                None
            }
        }
    }

    fn lower_and_load(&mut self, expr: &Expr, to: &mut Vec<ir::Op>) {
        if let Some(place) = self.lower_expr(expr, to) {
            self.load(&place, to);
        }
    }

    fn load(&self, place: &Place, to: &mut Vec<ir::Op>) {
        match place {
            Place::Global(i) => to.push(ir::Op::LoadGlobal(*i)),
            Place::Local(i) => to.push(ir::Op::LoadLocal(*i)),
            Place::Field(i) => to.push(ir::Op::LoadField(*i)),
            Place::Index => to.push(ir::Op::LoadIndex)
        }
    }

    fn store(&self, place: &Place, to: &mut Vec<ir::Op>) {
        match place {
            Place::Global(i) => to.push(ir::Op::StoreGlobal(*i)),
            Place::Local(i) => to.push(ir::Op::StoreLocal(*i)),
            Place::Field(i) => to.push(ir::Op::StoreField(*i)),
            Place::Index => to.push(ir::Op::StoreIndex)
        }
    }

    fn lower_stmt(&mut self, stmt: &Stmt, to: &mut Vec<ir::Op>) {
        match &stmt.kind {
            StmtKind::Expr(e) => {
                self.lower_and_load(e, to);
                to.push(ir::Op::Pop);
            },
            StmtKind::Assign(dest, val) => {
                let place = self.lower_expr(dest, to).unwrap();
                self.lower_and_load(val, to);
                self.store(&place, to);
            },
            StmtKind::Let(name, ty, val)
                | StmtKind::LetMut(name, ty, val) => {
                    self.lower_and_load(val, to);
                    to.push(ir::Op::CreateLocal);

                    self.stage.var_stack.push(IdentType {
                        span: stmt.span,
                        name: name.clone(),
                        ty: ty.clone()
                    });
                    self.stage.locals_in_frame += 1;
                },
            StmtKind::Return(val) => {
                self.lower_and_load(val, to);
                to.push(ir::Op::Ret);
            }
        }
    }
}

enum Place {
    Global(u32),
    Local(u32),
    Field(u32),
    Index
}
