//! Main evaluator for Ivy.

use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use ivy_syntax::{BinOp, Decl, Expr, FnBody, Literal, Program, Span, Spanned, TypeBody, UnaryOp};

use crate::builtins::*;
use crate::env::Env;
use crate::error::{EvalError, EvalResult};
use crate::loader::ModuleLoader;
use crate::pattern::match_pattern;
use crate::value::{vec_to_list, Closure, FnClause, ListValue, MultiClauseFn, Value};

/// The interpreter state.
pub struct Interpreter {
    /// Global environment.
    env: Env,
    /// Module loader for imports.
    loader: ModuleLoader,
    /// Loaded module namespaces: module_name -> (name -> value).
    modules: HashMap<String, HashMap<String, Value>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    /// Create a new interpreter with builtins.
    pub fn new() -> Self {
        let env = Env::new();

        env.define("__print", Value::Builtin(BUILTIN_PRINT.clone()), false);
        env.define("__println", Value::Builtin(BUILTIN_PRINTLN.clone()), false);
        env.define("__intToString", Value::Builtin(BUILTIN_INT_TO_STRING.clone()), false);
        env.define("__readLine", Value::Builtin(BUILTIN_READ_LINE.clone()), false);
        env.define("__readInt", Value::Builtin(BUILTIN_READ_INT.clone()), false);

        env.define("print", Value::Builtin(BUILTIN_PRINT.clone()), false);
        env.define("println", Value::Builtin(BUILTIN_PRINTLN.clone()), false);
        env.define("show", Value::Builtin(BUILTIN_SHOW.clone()), false);

        env.define("abs", Value::Builtin(BUILTIN_ABS.clone()), false);
        env.define("min", Value::Builtin(BUILTIN_MIN.clone()), false);
        env.define("max", Value::Builtin(BUILTIN_MAX.clone()), false);
        env.define("pow", Value::Builtin(BUILTIN_POW.clone()), false);
        env.define("sqrt", Value::Builtin(BUILTIN_SQRT.clone()), false);
        env.define("floor", Value::Builtin(BUILTIN_FLOOR.clone()), false);
        env.define("ceil", Value::Builtin(BUILTIN_CEIL.clone()), false);
        env.define("round", Value::Builtin(BUILTIN_ROUND.clone()), false);
        env.define("random", Value::Builtin(BUILTIN_RANDOM.clone()), false);

        env.define("strLength", Value::Builtin(BUILTIN_STR_LENGTH.clone()), false);
        env.define("strTrim", Value::Builtin(BUILTIN_STR_TRIM.clone()), false);
        env.define("strContains", Value::Builtin(BUILTIN_STR_CONTAINS.clone()), false);
        env.define("strSubstring", Value::Builtin(BUILTIN_STR_SUBSTRING.clone()), false);
        env.define("strSplit", Value::Builtin(BUILTIN_STR_SPLIT.clone()), false);
        env.define("strToUpper", Value::Builtin(BUILTIN_STR_TO_UPPER.clone()), false);
        env.define("strToLower", Value::Builtin(BUILTIN_STR_TO_LOWER.clone()), false);
        env.define("strStartsWith", Value::Builtin(BUILTIN_STR_STARTS_WITH.clone()), false);
        env.define("strEndsWith", Value::Builtin(BUILTIN_STR_ENDS_WITH.clone()), false);
        env.define("strReplace", Value::Builtin(BUILTIN_STR_REPLACE.clone()), false);

        env.define("readFile", Value::Builtin(BUILTIN_READ_FILE.clone()), false);
        env.define("writeFile", Value::Builtin(BUILTIN_WRITE_FILE.clone()), false);
        env.define("appendFile", Value::Builtin(BUILTIN_APPEND_FILE.clone()), false);
        env.define("fileExists", Value::Builtin(BUILTIN_FILE_EXISTS.clone()), false);

        env.define(
            "None",
            Value::Constructor {
                type_name: "Option".to_string(),
                variant: "None".to_string(),
                fields: vec![],
            },
            false,
        );
        env.define(
            "Some",
            Value::Constructor {
                type_name: "Option".to_string(),
                variant: "Some".to_string(),
                fields: vec![], // Will be populated when called
            },
            false,
        );

        env.define(
            "Ok",
            Value::Constructor {
                type_name: "Result".to_string(),
                variant: "Ok".to_string(),
                fields: vec![],
            },
            false,
        );
        env.define(
            "Err",
            Value::Constructor {
                type_name: "Result".to_string(),
                variant: "Err".to_string(),
                fields: vec![],
            },
            false,
        );

        env.define("true", Value::Bool(true), false);
        env.define("false", Value::Bool(false), false);

        let mut search_paths = vec![];

        if let Ok(cwd) = std::env::current_dir() {
            search_paths.push(cwd.clone());
            search_paths.push(cwd.join("lib"));
        }

        if let Ok(exe_path) = std::env::current_exe() {
            if let Some(exe_dir) = exe_path.parent() {
                search_paths.push(exe_dir.join("lib"));
                if let Some(parent) = exe_dir.parent() {
                    search_paths.push(parent.join("lib"));
                }
            }
        }

        let mut interp = Interpreter {
            env,
            loader: ModuleLoader::new(search_paths),
            modules: HashMap::new(),
        };

        interp.load_prelude();

        interp
    }

    /// Try to load the prelude file.
    fn load_prelude(&mut self) {
        let prelude_paths = [
            std::env::current_dir().ok().map(|d| d.join("lib/prelude.ivy")),
            std::env::current_exe()
                .ok()
                .and_then(|p| p.parent().map(|d| d.join("lib/prelude.ivy"))),
            std::env::current_exe()
                .ok()
                .and_then(|p| p.parent().and_then(|d| d.parent().map(|d| d.join("lib/prelude.ivy")))),
        ];

        for path_opt in prelude_paths {
            if let Some(path) = path_opt {
                if path.exists() {
                    if let Ok(source) = std::fs::read_to_string(&path) {
                        if let Ok(program) = ivy_parse::parse(&source) {
                            let mut public_names = std::collections::HashSet::new();
                            for decl in &program.declarations {
                                match &decl.node {
                                    Decl::Fn(fn_decl) if fn_decl.is_pub => {
                                        public_names.insert(fn_decl.name.name.clone());
                                    }
                                    Decl::Let {
                                        is_pub: true, pattern, ..
                                    } => {
                                        if let ivy_syntax::Pattern::Var(ident) = &pattern.node {
                                            public_names.insert(ident.name.clone());
                                        }
                                    }
                                    Decl::Type {
                                        is_pub: true,
                                        name,
                                        body,
                                        ..
                                    } => {
                                        public_names.insert(name.name.clone());
                                        if let ivy_syntax::TypeBody::Sum(variants) = body {
                                            for variant in variants {
                                                public_names.insert(variant.name.name.clone());
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            let grouped = self.collect_declarations(&program.declarations);
                            for decl in grouped {
                                let _ = self.eval_grouped_decl(&decl);
                            }
                        }
                    }
                    break;
                }
            }
        }
    }

    /// Add a search path for module resolution.
    pub fn add_search_path(&mut self, path: PathBuf) {
        self.loader.add_search_path(path);
    }

    /// Get a module namespace by name.
    pub fn get_module(&self, name: &str) -> Option<&HashMap<String, Value>> {
        self.modules.get(name)
    }

    /// List all user-defined bindings (excluding builtins).
    pub fn list_bindings(&self) -> Vec<String> {
        let builtins = [
            "__print",
            "__println",
            "__intToString",
            "__readLine",
            "__readInt",
            "print",
            "println",
            "show",
            "None",
            "Some",
            "Ok",
            "Err",
            "true",
            "false",
        ];
        self.env
            .list_bindings()
            .into_iter()
            .filter(|name| !builtins.contains(&name.as_str()))
            .collect()
    }

    pub fn eval_program(&mut self, program: &Program) -> EvalResult<Value> {
        for decl in &program.declarations {
            if let Decl::Import { path, items } = &decl.node {
                self.process_import(path, items.as_ref(), decl.span)?;
            }
        }

        let grouped = self.collect_declarations(&program.declarations);
        let mut last_value = Value::Unit;
        for decl in grouped {
            last_value = self.eval_grouped_decl(&decl)?;
        }
        Ok(last_value)
    }

    /// Process an import declaration.
    fn process_import(
        &mut self,
        path: &[ivy_syntax::Ident],
        items: Option<&Vec<ivy_syntax::Ident>>,
        span: Span,
    ) -> EvalResult<()> {
        if path.is_empty() {
            return Ok(());
        }

        let path_strings: Vec<String> = path.iter().map(|id| id.name.clone()).collect();
        let module_name = path_strings.join(".");

        if !self.modules.contains_key(&module_name) {
            match self.loader.load(&path_strings) {
                Ok(module) => {
                    let program = module.program.clone();
                    let public_names = module.public_names.clone();

                    let saved_env = std::mem::replace(&mut self.env, Env::new());

                    self.register_builtins();
                    let grouped = self.collect_declarations(&program.declarations);
                    for decl in grouped {
                        let _ = self.eval_grouped_decl(&decl);
                    }

                    let mut exports = HashMap::new();
                    for name in &public_names {
                        if let Some(value) = self.env.get(name) {
                            exports.insert(name.clone(), value);
                        }
                    }

                    self.env = saved_env;
                    self.modules.insert(module_name.clone(), exports);
                }
                Err(e) => {
                    return Err(EvalError::ModuleError {
                        message: e.to_string(),
                        span,
                    });
                }
            }
        }

        // Bring imported names into scope
        if let Some(module_exports) = self.modules.get(&module_name).cloned() {
            match items {
                Some(names) => {
                    // Selective import: `import Math.{add, sub}`
                    for name in names {
                        if let Some(value) = module_exports.get(&name.name) {
                            self.env.define(&name.name, value.clone(), false);
                        } else {
                            return Err(EvalError::PrivateItem {
                                name: name.name.clone(),
                                module: module_name.clone(),
                                span: name.span,
                            });
                        }
                    }
                }
                None => {
                    // // Full module import: `import Math`
                    self.env.define(
                        &path[0].name,
                        Value::Module {
                            name: module_name.clone(),
                        },
                        false,
                    );
                }
            }
        }

        Ok(())
    }

    /// Register builtins in the current environment.
    fn register_builtins(&self) {
        self.env.define("__print", Value::Builtin(BUILTIN_PRINT.clone()), false);
        self.env
            .define("__println", Value::Builtin(BUILTIN_PRINTLN.clone()), false);
        self.env
            .define("__intToString", Value::Builtin(BUILTIN_INT_TO_STRING.clone()), false);
        self.env
            .define("__readLine", Value::Builtin(BUILTIN_READ_LINE.clone()), false);
        self.env
            .define("__readInt", Value::Builtin(BUILTIN_READ_INT.clone()), false);
        self.env.define("print", Value::Builtin(BUILTIN_PRINT.clone()), false);
        self.env
            .define("println", Value::Builtin(BUILTIN_PRINTLN.clone()), false);
        self.env.define("show", Value::Builtin(BUILTIN_SHOW.clone()), false);
        self.env.define(
            "None",
            Value::Constructor {
                type_name: "Option".to_string(),
                variant: "None".to_string(),
                fields: vec![],
            },
            false,
        );
        self.env.define(
            "Some",
            Value::Constructor {
                type_name: "Option".to_string(),
                variant: "Some".to_string(),
                fields: vec![],
            },
            false,
        );
        self.env.define(
            "Ok",
            Value::Constructor {
                type_name: "Result".to_string(),
                variant: "Ok".to_string(),
                fields: vec![],
            },
            false,
        );
        self.env.define(
            "Err",
            Value::Constructor {
                type_name: "Result".to_string(),
                variant: "Err".to_string(),
                fields: vec![],
            },
            false,
        );
        self.env.define("true", Value::Bool(true), false);
        self.env.define("false", Value::Bool(false), false);
    }

    /// Evaluate a single expression (for REPL)
    pub fn eval_expr(&mut self, expr: &Spanned<Expr>) -> EvalResult<Value> {
        let span = expr.span;
        match &expr.node {
            Expr::Lit(lit) => Ok(literal_to_value(lit)),

            Expr::Var(ident) => self.env.get(&ident.name).ok_or_else(|| EvalError::UndefinedVariable {
                name: ident.name.clone(),
                span: ident.span,
            }),

            Expr::Binary { left, op, right } => self.eval_binary(left, &op.node, right, span),

            Expr::Unary { op, operand } => self.eval_unary(&op.node, operand, span),

            Expr::Let {
                is_mut, pattern, value, ..
            } => {
                let val = self.eval_expr(value)?;
                self.bind_pattern(&pattern.node, &val, *is_mut, pattern.span)?;
                Ok(val)
            }

            Expr::Assign { target, value } => self.eval_assign(target, value),

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.eval_expr(condition)?;
                match cond {
                    Value::Bool(true) => self.eval_expr(then_branch),
                    Value::Bool(false) => self.eval_expr(else_branch),
                    _ => Err(EvalError::TypeError {
                        expected: "Bool".to_string(),
                        found: cond.type_name(),
                        span: condition.span,
                    }),
                }
            }

            Expr::Match { scrutinee, arms } => {
                let value = self.eval_expr(scrutinee)?;
                for arm in arms {
                    if let Some(bindings) = match_pattern(&arm.pattern.node, &value) {
                        self.env.push_scope();
                        for (name, val) in bindings {
                            self.env.define(&name, val, false);
                        }
                        let result = self.eval_expr(&arm.body);
                        self.env.pop_scope();
                        return result;
                    }
                }
                Err(EvalError::MatchFailed { span })
            }

            Expr::Lambda { params, body, .. } => {
                let closure = Closure {
                    params: params.clone(),
                    body: (**body).clone(),
                    env: self.env.fork(),
                    name: None,
                };
                Ok(Value::Closure(Rc::new(closure)))
            }

            Expr::Call { callee, args } => {
                let func = self.eval_expr(callee)?;
                let arg_values: Vec<Value> = args.iter().map(|a| self.eval_expr(a)).collect::<EvalResult<_>>()?;
                self.apply(func, arg_values, span)
            }

            Expr::Field { object, field } => {
                let obj = self.eval_expr(object)?;
                self.access_field(&obj, &field.name, span)
            }

            Expr::Index { object, index } => {
                let obj = self.eval_expr(object)?;
                let idx = self.eval_expr(index)?;
                self.access_index(&obj, &idx, span)
            }

            Expr::Do { body } => {
                self.env.push_scope();
                let mut result = Value::Unit;
                for expr in body {
                    result = self.eval_expr(expr)?;
                }
                self.env.pop_scope();
                Ok(result)
            }

            Expr::Tuple { elements } => {
                let values: Vec<Value> = elements.iter().map(|e| self.eval_expr(e)).collect::<EvalResult<_>>()?;
                Ok(Value::Tuple(values))
            }

            Expr::List { elements } => {
                let values: Vec<Value> = elements.iter().map(|e| self.eval_expr(e)).collect::<EvalResult<_>>()?;
                Ok(vec_to_list(values))
            }

            Expr::Record { name, fields } => {
                let mut field_values = HashMap::new();
                for field in fields {
                    let val = self.eval_expr(&field.value)?;
                    field_values.insert(field.name.name.clone(), val);
                }
                Ok(Value::Record {
                    type_name: name.name.clone(),
                    fields: field_values,
                })
            }

            Expr::RecordUpdate { base, updates } => {
                let base_val = self.eval_expr(base)?;
                match base_val {
                    Value::Record { type_name, mut fields } => {
                        for update in updates {
                            let val = self.eval_expr(&update.value)?;
                            fields.insert(update.name.name.clone(), val);
                        }
                        Ok(Value::Record { type_name, fields })
                    }
                    _ => Err(EvalError::TypeError {
                        expected: "record".to_string(),
                        found: base_val.type_name(),
                        span: base.span,
                    }),
                }
            }

            Expr::Paren { inner } => self.eval_expr(inner),
        }
    }

    /// Evaluate binary operation.
    fn eval_binary(
        &mut self,
        left: &Spanned<Expr>,
        op: &BinOp,
        right: &Spanned<Expr>,
        span: Span,
    ) -> EvalResult<Value> {
        // Short-circuit for && and ||
        match op {
            BinOp::And => {
                let l = self.eval_expr(left)?;
                match l {
                    Value::Bool(false) => return Ok(Value::Bool(false)),
                    Value::Bool(true) => {
                        let r = self.eval_expr(right)?;
                        match r {
                            Value::Bool(b) => return Ok(Value::Bool(b)),
                            _ => {
                                return Err(EvalError::TypeError {
                                    expected: "Bool".to_string(),
                                    found: r.type_name(),
                                    span: right.span,
                                })
                            }
                        }
                    }
                    _ => {
                        return Err(EvalError::TypeError {
                            expected: "Bool".to_string(),
                            found: l.type_name(),
                            span: left.span,
                        })
                    }
                }
            }
            BinOp::Or => {
                let l = self.eval_expr(left)?;
                match l {
                    Value::Bool(true) => return Ok(Value::Bool(true)),
                    Value::Bool(false) => {
                        let r = self.eval_expr(right)?;
                        match r {
                            Value::Bool(b) => return Ok(Value::Bool(b)),
                            _ => {
                                return Err(EvalError::TypeError {
                                    expected: "Bool".to_string(),
                                    found: r.type_name(),
                                    span: right.span,
                                })
                            }
                        }
                    }
                    _ => {
                        return Err(EvalError::TypeError {
                            expected: "Bool".to_string(),
                            found: l.type_name(),
                            span: left.span,
                        })
                    }
                }
            }
            _ => {}
        }

        let l = self.eval_expr(left)?;
        let r = self.eval_expr(right)?;

        match op {
            BinOp::Add => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Sub => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Mul => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Div => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err(EvalError::DivisionByZero { span })
                    } else {
                        Ok(Value::Int(a / b))
                    }
                }
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Mod => match (&l, &r) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err(EvalError::DivisionByZero { span })
                    } else {
                        Ok(Value::Int(a % b))
                    }
                }
                _ => Err(EvalError::TypeError {
                    expected: "Int".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::Eq => Ok(Value::Bool(values_equal(&l, &r))),
            BinOp::Ne => Ok(Value::Bool(!values_equal(&l, &r))),
            BinOp::Lt => compare_values(&l, &r, |ord| ord == std::cmp::Ordering::Less, span),
            BinOp::Le => compare_values(&l, &r, |ord| ord != std::cmp::Ordering::Greater, span),
            BinOp::Gt => compare_values(&l, &r, |ord| ord == std::cmp::Ordering::Greater, span),
            BinOp::Ge => compare_values(&l, &r, |ord| ord != std::cmp::Ordering::Less, span),
            BinOp::Cons => match r {
                Value::List(list) => Ok(Value::List(Rc::new(ListValue::Cons(l, list)))),
                _ => Err(EvalError::TypeError {
                    expected: "List".to_string(),
                    found: r.type_name(),
                    span,
                }),
            },
            BinOp::Concat => match (&l, &r) {
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                (Value::List(a), Value::List(b)) => Ok(concat_lists(a, b)),
                _ => Err(EvalError::TypeError {
                    expected: "String or List".to_string(),
                    found: l.type_name(),
                    span,
                }),
            },
            BinOp::And | BinOp::Or => unreachable!(),
        }
    }

    /// Evaluate unary operation.
    fn eval_unary(&mut self, op: &UnaryOp, operand: &Spanned<Expr>, span: Span) -> EvalResult<Value> {
        let val = self.eval_expr(operand)?;
        match op {
            UnaryOp::Neg => match val {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(n) => Ok(Value::Float(-n)),
                _ => Err(EvalError::TypeError {
                    expected: "Int or Float".to_string(),
                    found: val.type_name(),
                    span,
                }),
            },
            UnaryOp::Not => match val {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(EvalError::TypeError {
                    expected: "Bool".to_string(),
                    found: val.type_name(),
                    span,
                }),
            },
        }
    }

    /// Evaluate assignment.
    fn eval_assign(&mut self, target: &Spanned<Expr>, value: &Spanned<Expr>) -> EvalResult<Value> {
        let val = self.eval_expr(value)?;
        match &target.node {
            Expr::Var(ident) => {
                self.env.assign(&ident.name, val.clone(), target.span)?;
                Ok(val)
            }
            _ => Err(EvalError::TypeError {
                expected: "variable".to_string(),
                found: "expression".to_string(),
                span: target.span,
            }),
        }
    }

    /// Bind a pattern to a value.
    fn bind_pattern(
        &mut self,
        pattern: &ivy_syntax::Pattern,
        value: &Value,
        is_mut: bool,
        span: Span,
    ) -> EvalResult<()> {
        match match_pattern(pattern, value) {
            Some(bindings) => {
                for (name, val) in bindings {
                    self.env.define(&name, val, is_mut);
                }
                Ok(())
            }
            None => Err(EvalError::MatchFailed { span }),
        }
    }

    /// Apply a function to arguments.
    fn apply(&mut self, func: Value, args: Vec<Value>, span: Span) -> EvalResult<Value> {
        match func {
            Value::Closure(closure) => {
                if args.len() != closure.params.len() {
                    return Err(EvalError::ArityMismatch {
                        expected: closure.params.len(),
                        got: args.len(),
                        span,
                    });
                }

                // Save current env, switch to closure's env
                let saved_env = std::mem::replace(&mut self.env, closure.env.fork());
                self.env.push_scope();

                // Bind parameters via pattern matching
                for (param, arg) in closure.params.iter().zip(args.iter()) {
                    match match_pattern(&param.pattern.node, arg) {
                        Some(bindings) => {
                            for (name, val) in bindings {
                                self.env.define(&name, val, false);
                            }
                        }
                        None => {
                            self.env = saved_env;
                            return Err(EvalError::MatchFailed { span });
                        }
                    }
                }

                // If named, allow recursion
                if let Some(ref name) = closure.name {
                    self.env.define(name, Value::Closure(closure.clone()), false);
                }

                let result = self.eval_expr(&closure.body);
                self.env = saved_env;
                result
            }

            Value::MultiClause(multi) => self.apply_multi_clause(&multi, args, span),

            Value::Builtin(builtin) => {
                if args.len() != builtin.arity {
                    return Err(EvalError::ArityMismatch {
                        expected: builtin.arity,
                        got: args.len(),
                        span,
                    });
                }
                (builtin.func)(&args)
            }

            Value::Constructor {
                type_name,
                variant,
                fields: _,
            } => Ok(Value::Constructor {
                type_name,
                variant,
                fields: args,
            }),

            _ => Err(EvalError::NotCallable {
                value_type: func.type_name(),
                span,
            }),
        }
    }

    /// Apply a multi-clause function.
    fn apply_multi_clause(&mut self, multi: &MultiClauseFn, args: Vec<Value>, span: Span) -> EvalResult<Value> {
        for clause in &multi.clauses {
            if clause.params.len() != args.len() {
                continue;
            }

            let mut all_bindings = vec![];
            let mut matched = true;

            for (param, arg) in clause.params.iter().zip(args.iter()) {
                match match_pattern(&param.pattern.node, arg) {
                    Some(bindings) => all_bindings.extend(bindings),
                    None => {
                        matched = false;
                        break;
                    }
                }
            }

            if matched {
                let saved_env = std::mem::replace(&mut self.env, multi.env.fork());
                self.env.push_scope();

                for (name, val) in all_bindings {
                    self.env.define(&name, val, false);
                }

                self.env
                    .define(&multi.name, Value::MultiClause(Rc::new((*multi).clone())), false);

                let result = match &clause.body {
                    FnBody::Expr(expr) => self.eval_expr(expr),
                    FnBody::Guards(guards) => {
                        let mut guard_result = None;
                        for guard in guards {
                            let cond = self.eval_expr(&guard.guard)?;
                            if matches!(cond, Value::Bool(true)) {
                                guard_result = Some(self.eval_expr(&guard.body)?);
                                break;
                            }
                        }
                        guard_result.ok_or(EvalError::MatchFailed { span })
                    }
                };

                self.env = saved_env;
                return result;
            }
        }

        Err(EvalError::MatchFailed { span })
    }

    /// Access a field on a value.
    fn access_field(&self, obj: &Value, field: &str, span: Span) -> EvalResult<Value> {
        match obj {
            Value::Record { type_name, fields } => fields.get(field).cloned().ok_or_else(|| EvalError::UnknownField {
                type_name: type_name.clone(),
                field: field.to_string(),
                span,
            }),
            Value::Tuple(elements) => {
                if let Ok(idx) = field.parse::<usize>() {
                    elements.get(idx).cloned().ok_or_else(|| EvalError::IndexOutOfBounds {
                        index: idx as i64,
                        length: elements.len(),
                        span,
                    })
                } else {
                    Err(EvalError::UnknownField {
                        type_name: "Tuple".to_string(),
                        field: field.to_string(),
                        span,
                    })
                }
            }
            Value::Module { name } => {
                if let Some(module_exports) = self.modules.get(name) {
                    module_exports
                        .get(field)
                        .cloned()
                        .ok_or_else(|| EvalError::PrivateItem {
                            name: field.to_string(),
                            module: name.clone(),
                            span,
                        })
                } else {
                    Err(EvalError::UndefinedModule {
                        name: name.clone(),
                        span,
                    })
                }
            }
            _ => Err(EvalError::TypeError {
                expected: "record, tuple, or module".to_string(),
                found: obj.type_name(),
                span,
            }),
        }
    }

    /// Access an index on a value.
    fn access_index(&self, obj: &Value, index: &Value, span: Span) -> EvalResult<Value> {
        match (obj, index) {
            (Value::List(list), Value::Int(idx)) => {
                let vec = list.to_vec();
                let i = if *idx < 0 {
                    (vec.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                vec.get(i).cloned().ok_or_else(|| EvalError::IndexOutOfBounds {
                    index: *idx,
                    length: vec.len(),
                    span,
                })
            }
            (Value::Tuple(elements), Value::Int(idx)) => {
                let i = *idx as usize;
                elements.get(i).cloned().ok_or_else(|| EvalError::IndexOutOfBounds {
                    index: *idx,
                    length: elements.len(),
                    span,
                })
            }
            (Value::String(s), Value::Int(idx)) => {
                let chars: Vec<char> = s.chars().collect();
                let i = if *idx < 0 {
                    (chars.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                chars
                    .get(i)
                    .map(|c| Value::Char(*c))
                    .ok_or_else(|| EvalError::IndexOutOfBounds {
                        index: *idx,
                        length: chars.len(),
                        span,
                    })
            }
            _ => Err(EvalError::TypeError {
                expected: "indexable type with Int index".to_string(),
                found: format!("{} with {}", obj.type_name(), index.type_name()),
                span,
            }),
        }
    }

    /// Collect and group declarations.
    fn collect_declarations(&self, decls: &[Spanned<Decl>]) -> Vec<GroupedDecl> {
        let mut result = Vec::new();
        let mut pending_fns: HashMap<String, Vec<FnClause>> = HashMap::new();
        let mut fn_order: Vec<String> = Vec::new();

        for decl in decls {
            match &decl.node {
                Decl::Fn(fn_decl) => {
                    let name = fn_decl.name.name.clone();
                    let clause = FnClause {
                        params: fn_decl.params.clone(),
                        body: fn_decl.body.clone(),
                    };

                    if !pending_fns.contains_key(&name) {
                        fn_order.push(name.clone());
                    }
                    pending_fns.entry(name).or_default().push(clause);
                }

                _ => {
                    for name in fn_order.drain(..) {
                        if let Some(clauses) = pending_fns.remove(&name) {
                            result.push(GroupedDecl::MultiClauseFn { name, clauses });
                        }
                    }
                    result.push(GroupedDecl::Single(decl.clone()));
                }
            }
        }

        for name in fn_order {
            if let Some(clauses) = pending_fns.remove(&name) {
                result.push(GroupedDecl::MultiClauseFn { name, clauses });
            }
        }

        result
    }

    /// Evaluate a grouped declaration.
    fn eval_grouped_decl(&mut self, decl: &GroupedDecl) -> EvalResult<Value> {
        match decl {
            GroupedDecl::Single(d) => self.eval_declaration(d),
            GroupedDecl::MultiClauseFn { name, clauses } => {
                let merged_clauses = if let Some(existing) = self.env.get(name) {
                    match existing {
                        Value::MultiClause(multi) => {
                            // Merge: existing clauses + new clauses
                            let mut all_clauses = multi.clauses.clone();
                            all_clauses.extend(clauses.clone());
                            all_clauses
                        }
                        Value::Closure(closure) => {
                            // Convert single closure to a clause and prepend
                            let existing_clause = FnClause {
                                params: closure.params.clone(),
                                body: FnBody::Expr(closure.body.clone()),
                            };
                            let mut all_clauses = vec![existing_clause];
                            all_clauses.extend(clauses.clone());
                            all_clauses
                        }
                        _ => clauses.clone(),
                    }
                } else {
                    clauses.clone()
                };

                let multi = MultiClauseFn {
                    name: name.clone(),
                    clauses: merged_clauses,
                    env: self.env.fork(),
                };
                self.env.define(name, Value::MultiClause(Rc::new(multi)), false);
                Ok(Value::Unit)
            }
        }
    }

    /// Evaluate a single declaration.
    fn eval_declaration(&mut self, decl: &Spanned<Decl>) -> EvalResult<Value> {
        match &decl.node {
            Decl::Module { .. } => Ok(Value::Unit),

            Decl::Import { .. } => Ok(Value::Unit),

            Decl::Type { name, body, .. } => {
                match body {
                    TypeBody::Sum(variants) => {
                        for variant in variants {
                            let arity = variant.fields.len();
                            if arity == 0 {
                                self.env.define(
                                    &variant.name.name,
                                    Value::Constructor {
                                        type_name: name.name.clone(),
                                        variant: variant.name.name.clone(),
                                        fields: vec![],
                                    },
                                    false,
                                );
                            } else {
                                self.env.define(
                                    &variant.name.name,
                                    Value::Constructor {
                                        type_name: name.name.clone(),
                                        variant: variant.name.name.clone(),
                                        fields: vec![],
                                    },
                                    false,
                                );
                            }
                        }
                    }
                    TypeBody::Record(_) => {
                        // Record types don't need special constructor registration
                    }
                }
                Ok(Value::Unit)
            }

            // TODO(gtr): we do not currently support traits. Next biggest win
            Decl::Trait { .. } => Ok(Value::Unit),
            Decl::Impl { .. } => Ok(Value::Unit),

            Decl::Let {
                is_mut, pattern, value, ..
            } => {
                let val = self.eval_expr(value)?;
                self.bind_pattern(&pattern.node, &val, *is_mut, pattern.span)?;
                Ok(val)
            }

            Decl::Fn(fn_decl) => {
                let closure = Closure {
                    params: fn_decl.params.clone(),
                    body: match &fn_decl.body {
                        FnBody::Expr(e) => e.clone(),
                        FnBody::Guards(_) => {
                            return Ok(Value::Unit);
                        }
                    },
                    env: self.env.fork(),
                    name: Some(fn_decl.name.name.clone()),
                };
                self.env
                    .define(&fn_decl.name.name, Value::Closure(Rc::new(closure)), false);
                Ok(Value::Unit)
            }

            // Type signatures (were) handled by the type checker, noop in runtime
            Decl::TypeSig { .. } => Ok(Value::Unit),
        }
    }
}

/// Grouped declaration after collecting.
enum GroupedDecl {
    Single(Spanned<Decl>),
    MultiClauseFn { name: String, clauses: Vec<FnClause> },
}

/// Convert a literal to a value.
fn literal_to_value(lit: &Literal) -> Value {
    match lit {
        Literal::Int(n) => Value::Int(*n),
        Literal::Float(n) => Value::Float(*n),
        Literal::String(s) => Value::String(s.clone()),
        Literal::Char(c) => Value::Char(*c),
        Literal::Bool(b) => Value::Bool(*b),
        Literal::Unit => Value::Unit,
    }
}

/// Check if two values are equal.
fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Unit, Value::Unit) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => (x - y).abs() < f64::EPSILON,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Tuple(xs), Value::Tuple(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::List(xs), Value::List(ys)) => lists_equal(xs, ys),
        (
            Value::Constructor {
                variant: v1,
                fields: f1,
                ..
            },
            Value::Constructor {
                variant: v2,
                fields: f2,
                ..
            },
        ) => v1 == v2 && f1.len() == f2.len() && f1.iter().zip(f2.iter()).all(|(x, y)| values_equal(x, y)),
        _ => false,
    }
}

/// Check if two lists are equal.
fn lists_equal(a: &ListValue, b: &ListValue) -> bool {
    match (a, b) {
        (ListValue::Nil, ListValue::Nil) => true,
        (ListValue::Cons(h1, t1), ListValue::Cons(h2, t2)) => values_equal(h1, h2) && lists_equal(t1, t2),
        _ => false,
    }
}

/// Compare two values.
fn compare_values<F>(a: &Value, b: &Value, cmp: F, span: Span) -> EvalResult<Value>
where
    F: Fn(std::cmp::Ordering) -> bool,
{
    let ord = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x.cmp(y),
        (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
        (Value::String(x), Value::String(y)) => x.cmp(y),
        (Value::Char(x), Value::Char(y)) => x.cmp(y),
        _ => {
            return Err(EvalError::TypeError {
                expected: "comparable types".to_string(),
                found: format!("{} and {}", a.type_name(), b.type_name()),
                span,
            })
        }
    };
    Ok(Value::Bool(cmp(ord)))
}

/// Concatenate two lists.
fn concat_lists(a: &ListValue, b: &Rc<ListValue>) -> Value {
    match a {
        ListValue::Nil => Value::List(b.clone()),
        ListValue::Cons(head, tail) => {
            let new_tail = match concat_lists(tail, b) {
                Value::List(l) => l,
                _ => unreachable!(),
            };
            Value::List(Rc::new(ListValue::Cons(head.clone(), new_tail)))
        }
    }
}
