use std::{cell::RefCell, sync::Arc};

#[derive(Debug)]
pub struct Interpreter {
    functions: Vec<Function>,
    vm: Vm
}

#[derive(Debug)]
struct Vm {
    globals: Vec<Vec<Value>>,
    locals: Vec<Value>,
    stack: Vec<Value>,
    current_namespace: u32
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            vm: Vm {
                globals: Vec::new(),
                locals: Vec::new(),
                stack: Vec::new(),
                current_namespace: 0
            }
        }
    }

    pub fn add_function(&mut self, func: Function) -> u32 {
        self.functions.push(func);

        return self.functions.len() as u32 - 1;
    }

    pub fn add_namespace(&mut self, num_values: usize) -> u32 {
        self.vm.globals.push(vec![Value::Unit; num_values]);

        return self.vm.globals.len() as u32 - 1;
    }

    pub fn set_global(&mut self, ns: u32, id: u32, value: Value) {
        self.vm.globals[ns as usize][id as usize] = value;
    }

    pub fn invoke_function(
        &mut self,
        name: &str,
        stack: Vec<Value>
    ) -> Vec<Value> {
        self.vm.stack = stack;

        let mut func_id = None;

        for (id, func) in self.functions.iter().enumerate() {
            if name == &func.name {
                func_id = Some(id as u32);
            }
        }

        let func_id = func_id.unwrap();

        self.vm.call_function(&self.functions, func_id);

        return std::mem::replace(&mut self.vm.stack, Vec::new());
    }
}

impl Vm {
    fn call_function(&mut self, functions: &[Function], id: u32) {
        let func = &functions[id as usize];

        let last_namespace = self.current_namespace;
        self.current_namespace = func.globals_namespace;

        for _ in 0..func.params {
            self.locals.push(Value::Unit);
        }

        let n = self.locals.len();
        for i in 0..func.params {
            self.locals[n - 1 - i as usize] = self.stack.pop().unwrap();
        }

        // i really want to pull this out into its own function
        // but that would cause borrow checker issues.
        // ideally i should split the `VM` struct into separate structs
        // for stack, globals, functions, etc.
        // and then this could mutably borrow the stack and variables
        // without mutably aliasing the code.
        let mut ip: usize = 0;
        let mut locals_in_frame = func.params as usize;

        loop {
            match &func.code[ip as usize] {
                Op::ConstUnit => self.stack.push(Value::Unit),
                Op::ConstFalse => self.stack.push(Value::Bool(false)),
                Op::ConstTrue => self.stack.push(Value::Bool(true)),
                Op::ConstInt(i) => self.stack.push(Value::Int(*i)),
                Op::ConstUint(u) => self.stack.push(Value::Uint(*u)),
                Op::ConstructObject(n) => {
                    let fields: Arc<[RefCell<Value>]> = vec![
                        RefCell::new(Value::Unit); *n as usize
                    ].into();

                    for i in 0..*n {
                        *fields[(*n - 1 - i) as usize].borrow_mut() =
                            self.stack.pop().unwrap();
                    }

                    self.stack.push(Value::Object(fields));
                },
                Op::ConstructArray(n) => {
                    let arr = vec![RefCell::new(Value::Unit); *n as usize];

                    for i in 0..*n {
                        *arr[(*n - 1 - i) as usize].borrow_mut() =
                            self.stack.pop().unwrap();
                    }

                    self.stack.push(Value::Array(Arc::new(RefCell::new(arr))));
                },
                Op::LoadField(i) => {
                    let Value::Object(fields) = self.stack.pop().unwrap() else {
                        panic!("Not an object");
                    };

                    self.stack.push(fields[*i as usize].borrow().clone());
                },
                Op::StoreField(i) => {
                    let value = self.stack.pop().unwrap();

                    let Value::Object(fields) = self.stack.pop().unwrap() else {
                        panic!("Not an object");
                    };

                    *fields[*i as usize].borrow_mut() = value;
                },
                Op::LoadIndex => {
                    let Value::Uint(idx) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };
                    let Value::Array(arr) = self.stack.pop().unwrap() else {
                        unimplemented!();
                    };

                    self.stack.push(arr.borrow()[idx as usize].borrow().clone());
                },
                Op::StoreIndex => {
                    let value = self.stack.pop().unwrap();
                    let Value::Uint(idx) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };
                    let Value::Array(arr) = self.stack.pop().unwrap() else {
                        unimplemented!();
                    };

                    *arr.borrow_mut()[idx as usize].borrow_mut() = value;
                },
                Op::CreateLocal => {
                    self.locals.push(self.stack.pop().unwrap());
                    locals_in_frame += 1;
                },
                Op::DestroyLocals(n) => {
                    for _ in 0..*n {
                        self.locals.pop();
                    }

                    locals_in_frame -= *n as usize;
                },
                Op::LoadLocal(i) => {
                    self.stack.push(
                        self.locals[self.locals.len() - 1 - *i as usize].clone()
                    );
                },
                Op::StoreLocal(i) => {
                    let value = self.stack.pop().unwrap();
                    let idx = self.locals.len() - 1 - *i as usize;
                    self.locals[idx] = value;
                },
                Op::LoadGlobal(i) => {
                    self.stack.push(
                        self.globals[
                            self.current_namespace as usize
                        ][*i as usize].clone()
                    );
                },
                Op::StoreGlobal(i) => {
                    let value = self.stack.pop().unwrap();
                    self.globals[
                        self.current_namespace as usize
                    ][*i as usize] = value;
                },
                Op::Add => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Int(x + y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Uint(x + y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Sub => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Int(x - y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Uint(x - y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Mul => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Int(x * y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Uint(x * y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Div => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Int(x / y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Uint(x / y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Mod => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Int(x % y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Uint(x % y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Eq => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    self.stack.push(Value::Bool(x == y));
                },
                Op::Ne => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    self.stack.push(Value::Bool(x != y));
                },
                Op::Lt => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x < y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x < y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Le => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x <= y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x <= y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Gt => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x > y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x > y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::Ge => {
                    let y = self.stack.pop().unwrap();
                    let x = self.stack.pop().unwrap();

                    match x {
                        Value::Int(x) => {
                            let Value::Int(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x >= y));
                        },
                        Value::Uint(x) => {
                            let Value::Uint(y) = y else { unimplemented!() };

                            self.stack.push(Value::Bool(x >= y));
                        },
                        _ => unimplemented!()
                    }
                },
                Op::And => {
                    let Value::Bool(y) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };
                    let Value::Bool(x) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };

                    self.stack.push(Value::Bool(x && y));
                },
                Op::Or => {
                    let Value::Bool(y) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };
                    let Value::Bool(x) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };

                    self.stack.push(Value::Bool(x || y));
                },
                Op::Call => {
                    let Value::Function(f) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };

                    self.call_function(functions, f);
                },
                Op::Ret => break,
                Op::Try => {
                    let val = self.stack.pop().unwrap();

                    match val {
                        Value::Option(opt) => {
                            if opt.borrow().is_none() {
                                self.stack.push(Value::Option(opt));
                                break;
                            } else {
                                self.stack.push(opt.borrow().clone().unwrap());
                            }
                        },
                        Value::Result(res) => {
                            match &*res.borrow() {
                                Ok(ok) => {
                                    self.stack.push(ok.clone());
                                },
                                Err(_) => {
                                    self.stack.push(Value::Result(res.clone()));
                                    break;
                                }
                            }
                        }
                        _ => unimplemented!()
                    }
                },
                Op::Jump(offset) => {
                    if *offset < 0 {
                        ip -= (-*offset) as usize;
                    } else {
                        ip += *offset as usize;
                    }

                    continue;
                },
                Op::JumpIfTrue(offset) => {
                    let Value::Bool(cond) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };

                    if cond {
                        if *offset < 0 {
                            ip -= (-*offset) as usize;
                        } else {
                            ip += *offset as usize;
                        }

                        continue;
                    }
                },
                Op::JumpIfFalse(offset) => {
                    let Value::Bool(cond) = self.stack.pop().unwrap() else {
                        unimplemented!()
                    };

                    if !cond {
                        if *offset < 0 {
                            ip -= (-*offset) as usize;
                        } else {
                            ip += *offset as usize;
                        }

                        continue;
                    }
                },
                Op::JumpIfSome(offset) => {
                    match self.stack.pop().unwrap() {
                        Value::Option(opt) => {
                            if let Some(val) = opt.borrow().clone() {
                                if *offset < 0 {
                                    ip -= (-*offset) as usize;
                                } else {
                                    ip += *offset as usize;
                                }

                                self.stack.push(val);

                                continue;
                            }
                        },
                        Value::Result(res) => {
                            match res.borrow().clone() {
                                Ok(ok) => {
                                    if *offset < 0 {
                                        ip -= (-*offset) as usize;
                                    } else {
                                        ip += *offset as usize;
                                    }

                                    self.stack.push(ok);

                                    continue;
                                },
                                Err(err) => {
                                    self.stack.push(err);
                                }
                            }
                        },
                        _ => unimplemented!()
                    }
                },
                _ => todo!()
            }

            ip += 1;
        }

        for _ in 0..locals_in_frame {
            self.locals.pop();
        }

        self.current_namespace = last_namespace;
    }
}

pub type Ref<T> = Arc<RefCell<T>>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i32),
    Uint(u32),
    String(Ref<String>),
    Object(Arc<[RefCell<Value>]>),
    Option(Ref<Option<Value>>),
    Result(Ref<Result<Value, Value>>),
    Array(Ref<Vec<RefCell<Value>>>),
    Function(u32)
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub globals_namespace: u32,
    pub params: u32,
    pub code: Vec<Op>
}

#[derive(Clone, Debug)]
pub enum Op {
    Pop,

    ConstUnit,

    ConstTrue,
    ConstFalse,

    ConstInt(i32),
    ConstUint(u32),
    ConstString(u32),

    ConstructObject(u32),
    LoadField(u32),
    StoreField(u32),

    ConstructArray(u32),
    LoadIndex,
    StoreIndex,

    CreateLocal,
    DestroyLocals(u32),
    LoadLocal(u32),
    StoreLocal(u32),

    LoadGlobal(u32),
    StoreGlobal(u32),

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    And,
    Or,

    Call,
    Ret,
    Try,

    Jump(i32),
    JumpIfTrue(i32),
    JumpIfFalse(i32),

    JumpIfSome(i32)
}
