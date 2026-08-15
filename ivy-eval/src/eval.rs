//! Main evaluator for Ivy.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::{env, fs, mem};

use ivy_parse::ModuleLoader;
use ivy_syntax::decl::{FnDecl, ImportKind, TraitItem};
use ivy_syntax::types::TypeExpr;
use ivy_syntax::{Decl, Expr, FnBody, Program, Span, Spanned, TypeBody};

use crate::builtins::*;
use crate::env::Env;
use crate::error::{EvalError, EvalResult};
use crate::eval_ops::literal_to_value;
use crate::pattern::match_pattern;
use crate::value::{vec_to_list, Closure, DispatchTag, FnClause, MultiClauseFn, RecursionMode, Value};

/// The interpreter state.
pub struct Interpreter {
    /// Global environment.
    pub(crate) env: Env,
    /// Loaded module namespaces: module_name -> (name -> value).
    pub(crate) modules: HashMap<String, HashMap<String, Value>>,
    /// Modules currently being loaded (for cycle detection).
    loaded_modules: HashSet<String>,
    /// Trait method implementations keyed by `(trait_name, type_tag)` -> `method_name` -> clauses
    pub(crate) trait_impls: HashMap<(String, String), HashMap<String, Vec<FnClause>>>,
    /// Trait default impls keyed by `trait_name` -> `method_name` -> default clauses
    pub(crate) trait_defaults: HashMap<String, HashMap<String, Vec<FnClause>>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    /// Create a new interpreter with builtins and prelude (for tests and standalone usage).
    pub fn new() -> Self {
        let mut interp = Self::with_builtins();

        let prelude_paths = [
            env::current_dir().ok().map(|d| d.join("lib/prelude.ivy")),
            env::current_exe()
                .ok()
                .and_then(|p| p.parent().map(|d| d.join("lib/prelude.ivy"))),
            env::current_exe()
                .ok()
                .and_then(|p| p.parent().and_then(|d| d.parent().map(|d| d.join("lib/prelude.ivy")))),
        ];

        for path in prelude_paths.into_iter().flatten() {
            if path.exists() {
                if let Ok(source) = fs::read_to_string(&path) {
                    if let Ok(program) = ivy_parse::parse(&source) {
                        interp.load_program(&program);
                    }
                }
                break;
            }
        }

        interp
    }

    /// Create an interpreter with builtins but no prelude.
    pub fn with_builtins() -> Self {
        let env = Env::new();
        let interp = Interpreter {
            env,
            modules: HashMap::new(),
            loaded_modules: HashSet::new(),
            trait_impls: HashMap::new(),
            trait_defaults: HashMap::new(),
        };
        interp.register_builtins();
        interp
    }

    /// Load a program's declarations into the interpreter (for prelude loading).
    pub fn load_program(&mut self, program: &Program) {
        let grouped = self.collect_declarations(&program.declarations);
        for decl in grouped {
            let _ = self.eval_grouped_decl(&decl);
        }
    }

    /// Get a loaded module by name, if it exists.
    pub fn get_loaded_module(&self, module_name: &str) -> Option<&HashMap<String, Value>> {
        self.modules.get(module_name)
    }

    /// Get a module namespace by name.
    pub fn get_module(&self, name: &str) -> Option<&HashMap<String, Value>> {
        self.modules.get(name)
    }

    /// List exports for modules that are fully imported
    pub fn list_module_exports(&self) -> Vec<(String, Vec<String>)> {
        let imported_modules: Vec<String> = self
            .env
            .all_bindings()
            .into_iter()
            .filter_map(|(_name, value)| {
                if let Value::Module { name: module_name } = value {
                    Some(module_name)
                } else {
                    None
                }
            })
            .collect();

        self.modules
            .iter()
            .filter(|(module_name, _)| imported_modules.contains(module_name))
            .map(|(module_name, exports)| {
                let mut export_names: Vec<String> = exports.keys().cloned().collect();
                export_names.sort();
                (module_name.clone(), export_names)
            })
            .collect()
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

    /// Evaluate a program with a default loader (for tests and simple usage).
    pub fn eval_program(&mut self, program: &Program) -> EvalResult<Value> {
        let mut loader = ModuleLoader::new(ivy_utils::get_default_search_paths());
        self.eval_program_with_loader(program, &mut loader)
    }

    /// Evaluate a program with a shared module loader.
    pub fn eval_program_with_loader(&mut self, program: &Program, loader: &mut ModuleLoader) -> EvalResult<Value> {
        for decl in &program.declarations {
            if let Decl::Import { path, kind } = &decl.node {
                self.process_import(path, kind, decl.span, loader)?;
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
        kind: &ImportKind,
        span: Span,
        loader: &mut ModuleLoader,
    ) -> EvalResult<()> {
        if path.is_empty() {
            return Ok(());
        }

        let path_strings: Vec<String> = path.iter().map(|id| id.name.clone()).collect();
        let module_name = path_strings.join(".");

        if self.loaded_modules.contains(&module_name) {
            let mut cycle: Vec<String> = self.loaded_modules.iter().cloned().collect();
            cycle.push(module_name.clone());
            return Err(EvalError::CircularImport {
                module: module_name,
                cycle,
                span,
            });
        }

        if !self.modules.contains_key(&module_name) {
            self.loaded_modules.insert(module_name.clone());

            let parsed = loader.load(&path_strings).map_err(|e| EvalError::ModuleError {
                message: e.to_string(),
                span,
            })?;

            let program = parsed.program.clone();
            let public_names = parsed.public_names.clone();

            let saved_env = mem::take(&mut self.env);

            self.register_builtins();

            // Process nested imports
            for decl in &program.declarations {
                if let Decl::Import {
                    path: imp_path,
                    kind: imp_kind,
                } = &decl.node
                {
                    self.process_import(imp_path, imp_kind, decl.span, loader)?;
                }
            }

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
            self.loaded_modules.remove(&module_name);
        }

        if let Some(module_exports) = self.modules.get(&module_name).cloned() {
            match kind {
                ImportKind::Qualified => {
                    self.env.define(
                        &path[0].name,
                        Value::Module {
                            name: module_name.clone(),
                        },
                        false,
                    );
                }
                ImportKind::Alias(alias) => {
                    self.env.define(
                        &alias.name,
                        Value::Module {
                            name: module_name.clone(),
                        },
                        false,
                    );
                }
                ImportKind::All => {
                    for (name, value) in module_exports {
                        self.env.define(&name, value.clone(), false);
                    }
                }
                ImportKind::Items(items) => {
                    for item in items {
                        if let Some(value) = module_exports.get(&item.name) {
                            self.env.define(&item.name, value.clone(), false);
                        } else {
                            return Err(EvalError::PrivateItem {
                                name: item.name.clone(),
                                module: module_name.clone(),
                                span: item.span,
                            });
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Register builtins in the current environment.
    fn register_builtins(&self) {
        // True builtins (user-facing, always available)
        self.env.define("print", Value::Builtin(BUILTIN_PRINT.clone()), false);
        self.env
            .define("println", Value::Builtin(BUILTIN_PRINTLN.clone()), false);
        self.env.define("show", Value::Builtin(BUILTIN_SHOW.clone()), false);

        // I/O intrinsics
        self.env.define("__print", Value::Builtin(BUILTIN_PRINT.clone()), false);
        self.env
            .define("__println", Value::Builtin(BUILTIN_PRINTLN.clone()), false);
        self.env
            .define("__intToString", Value::Builtin(BUILTIN_INT_TO_STRING.clone()), false);
        self.env
            .define("__charToString", Value::Builtin(BUILTIN_CHAR_TO_STRING.clone()), false);
        self.env
            .define("__intEq", Value::Builtin(BUILTIN_INT_EQ.clone()), false);
        self.env
            .define("__floatEq", Value::Builtin(BUILTIN_FLOAT_EQ.clone()), false);
        self.env
            .define("__strEq", Value::Builtin(BUILTIN_STR_EQ.clone()), false);
        self.env
            .define("__intCompare", Value::Builtin(BUILTIN_INT_COMPARE.clone()), false);
        self.env
            .define("__floatCompare", Value::Builtin(BUILTIN_FLOAT_COMPARE.clone()), false);
        self.env
            .define("__strCompare", Value::Builtin(BUILTIN_STR_COMPARE.clone()), false);
        self.env
            .define("__readLine", Value::Builtin(BUILTIN_READ_LINE.clone()), false);
        self.env
            .define("__readInt", Value::Builtin(BUILTIN_READ_INT.clone()), false);

        // Conversion intrinsics (wrapped by lib/Convert.ivy)
        self.env
            .define("__floatFromInt", Value::Builtin(BUILTIN_FLOAT_FROM_INT.clone()), false);
        self.env
            .define("__floatToInt", Value::Builtin(BUILTIN_FLOAT_TO_INT.clone()), false);
        self.env.define(
            "__floatToString",
            Value::Builtin(BUILTIN_FLOAT_TO_STRING.clone()),
            false,
        );
        self.env
            .define("__stringToInt", Value::Builtin(BUILTIN_STRING_TO_INT.clone()), false);
        self.env.define(
            "__stringToFloat",
            Value::Builtin(BUILTIN_STRING_TO_FLOAT.clone()),
            false,
        );
        self.env.define(
            "__tryStringToInt",
            Value::Builtin(BUILTIN_TRY_STRING_TO_INT.clone()),
            false,
        );
        self.env.define(
            "__tryStringToFloat",
            Value::Builtin(BUILTIN_TRY_STRING_TO_FLOAT.clone()),
            false,
        );

        // Math intrinsics (wrapped by lib/Math.ivy)
        self.env.define("__abs", Value::Builtin(BUILTIN_ABS.clone()), false);
        self.env.define("__min", Value::Builtin(BUILTIN_MIN.clone()), false);
        self.env.define("__max", Value::Builtin(BUILTIN_MAX.clone()), false);
        self.env.define("__pow", Value::Builtin(BUILTIN_POW.clone()), false);
        self.env.define("__sqrt", Value::Builtin(BUILTIN_SQRT.clone()), false);
        self.env.define("__floor", Value::Builtin(BUILTIN_FLOOR.clone()), false);
        self.env.define("__ceil", Value::Builtin(BUILTIN_CEIL.clone()), false);
        self.env.define("__round", Value::Builtin(BUILTIN_ROUND.clone()), false);
        self.env
            .define("__random", Value::Builtin(BUILTIN_RANDOM.clone()), false);

        // String intrinsics (wrapped by lib/String.ivy)
        self.env
            .define("__strLength", Value::Builtin(BUILTIN_STR_LENGTH.clone()), false);
        self.env
            .define("__strTrim", Value::Builtin(BUILTIN_STR_TRIM.clone()), false);
        self.env
            .define("__strContains", Value::Builtin(BUILTIN_STR_CONTAINS.clone()), false);
        self.env
            .define("__strSubstring", Value::Builtin(BUILTIN_STR_SUBSTRING.clone()), false);
        self.env
            .define("__strSplit", Value::Builtin(BUILTIN_STR_SPLIT.clone()), false);
        self.env
            .define("__strToUpper", Value::Builtin(BUILTIN_STR_TO_UPPER.clone()), false);
        self.env
            .define("__strToLower", Value::Builtin(BUILTIN_STR_TO_LOWER.clone()), false);
        self.env.define(
            "__strStartsWith",
            Value::Builtin(BUILTIN_STR_STARTS_WITH.clone()),
            false,
        );
        self.env
            .define("__strEndsWith", Value::Builtin(BUILTIN_STR_ENDS_WITH.clone()), false);
        self.env
            .define("__strReplace", Value::Builtin(BUILTIN_STR_REPLACE.clone()), false);

        // File I/O intrinsics (wrapped by lib/File.ivy)
        self.env
            .define("__readFile", Value::Builtin(BUILTIN_READ_FILE.clone()), false);
        self.env
            .define("__writeFile", Value::Builtin(BUILTIN_WRITE_FILE.clone()), false);
        self.env
            .define("__appendFile", Value::Builtin(BUILTIN_APPEND_FILE.clone()), false);
        self.env
            .define("__fileExists", Value::Builtin(BUILTIN_FILE_EXISTS.clone()), false);

        // Constructors
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

            Expr::Binary { left, op, right } => self.eval_binary(left, op.node, right, span),

            Expr::Unary { op, operand } => self.eval_unary(op.node, operand, span),

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

    fn register_trait(&mut self, trait_name: &str, items: &[TraitItem]) {
        let mut defaults: HashMap<String, Vec<FnClause>> = HashMap::new();
        for item in items {
            let method_name = match item {
                TraitItem::Signature { name, .. } => name.name.clone(),
                TraitItem::DefaultImpl(fn_decl) => fn_decl.name.name.clone(),
            };
            self.env.define(
                &method_name,
                Value::TraitMethod {
                    trait_name: trait_name.to_string(),
                    method: method_name.clone(),
                },
                false,
            );
            if let TraitItem::DefaultImpl(fn_decl) = item {
                defaults.insert(
                    method_name,
                    vec![FnClause {
                        params: fn_decl.params.clone(),
                        body: fn_decl.body.clone(),
                    }],
                );
            }
        }
        self.trait_defaults.insert(trait_name.to_string(), defaults);
    }

    fn register_impl(&mut self, trait_name: &str, for_type: &TypeExpr, methods: &[Spanned<FnDecl>]) {
        let tag = for_type.dispatch_tag();
        let grouped = group_fn_clauses(methods.iter().map(|m| &m.node));
        self.trait_impls.insert((trait_name.to_string(), tag), grouped);
    }

    /// Look up the impl method for a trait dispatched on `arg`'s runtime type
    fn lookup_trait_impl(&self, trait_name: &str, method: &str, arg: &Value) -> Option<Value> {
        let tag = arg.dispatch_tag();
        let trait_key = trait_name.to_string();
        let clauses = self
            .trait_impls
            .get(&(trait_key.clone(), tag))
            .and_then(|m| m.get(method))
            .or_else(|| self.trait_defaults.get(&trait_key).and_then(|m| m.get(method)))?;
        Some(self.build_method_value(method, clauses))
    }

    fn build_method_value(&self, method: &str, clauses: &[FnClause]) -> Value {
        if let [only] = clauses {
            if let FnBody::Expr(e) = &only.body {
                return Value::Closure(Rc::new(Closure {
                    params: only.params.clone(),
                    body: e.clone(),
                    env: self.env.fork(),
                    name: None,
                }));
            }
        }
        Value::MultiClause(Rc::new(MultiClauseFn {
            name: method.to_string(),
            clauses: clauses.to_vec(),
            env: self.env.fork(),
            recursion: RecursionMode::ThroughEnv,
        }))
    }

    fn apply(&mut self, func: Value, args: Vec<Value>, span: Span) -> EvalResult<Value> {
        match func {
            Value::TraitMethod {
                ref trait_name,
                ref method,
            } => {
                if args.is_empty() {
                    return Ok(Value::PartialApp {
                        func: Box::new(func.clone()),
                        applied_args: args,
                    });
                }
                let dispatched =
                    self.lookup_trait_impl(trait_name, method, &args[0])
                        .ok_or_else(|| EvalError::TypeError {
                            expected: format!("a value with `impl {} for ...`", trait_name),
                            found: args[0].type_name(),
                            span,
                        })?;
                self.apply(dispatched, args, span)
            }
            Value::PartialApp {
                func: inner_func,
                applied_args,
            } => {
                let mut all_args = applied_args;
                all_args.extend(args);
                self.apply(*inner_func, all_args, span)
            }

            Value::Closure(ref closure) => {
                let arity = closure.params.len();

                if args.len() < arity {
                    return Ok(Value::PartialApp {
                        func: Box::new(func),
                        applied_args: args,
                    });
                }

                if args.len() > arity {
                    let (now, later) = args.split_at(arity);
                    let result = self.apply_closure(closure, now, span)?;
                    return self.apply(result, later.to_vec(), span);
                }

                self.apply_closure(closure, &args, span)
            }

            Value::MultiClause(ref multi) => {
                let arity = self.multi_clause_arity(multi);

                if args.len() < arity {
                    return Ok(Value::PartialApp {
                        func: Box::new(func),
                        applied_args: args,
                    });
                }

                if args.len() > arity {
                    let (now, later) = args.split_at(arity);
                    let result = self.apply_multi_clause(multi, now, span)?;
                    return self.apply(result, later.to_vec(), span);
                }

                self.apply_multi_clause(multi, &args, span)
            }

            Value::Builtin(ref builtin) => {
                let arity = builtin.arity;

                if args.len() < arity {
                    return Ok(Value::PartialApp {
                        func: Box::new(func),
                        applied_args: args,
                    });
                }

                if args.len() > arity {
                    let (now, later) = args.split_at(arity);
                    let result = (builtin.func)(now)?;
                    return self.apply(result, later.to_vec(), span);
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

    /// Apply a closure with exact number of arguments
    fn apply_closure(&mut self, closure: &Rc<Closure>, args: &[Value], span: Span) -> EvalResult<Value> {
        let saved_env = mem::replace(&mut self.env, closure.env.fork());
        self.env.push_scope();

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
        if let Some(ref name) = closure.name {
            self.env.define(name, Value::Closure(closure.clone()), false);
        }
        let result = self.eval_expr(&closure.body);
        self.env = saved_env;
        result
    }

    /// Get the arity of a multi-clause function (uses first clause)
    fn multi_clause_arity(&self, multi: &MultiClauseFn) -> usize {
        multi.clauses.first().map(|c| c.params.len()).unwrap_or(0)
    }

    /// Apply a multi-clause function.
    fn apply_multi_clause(&mut self, multi: &MultiClauseFn, args: &[Value], span: Span) -> EvalResult<Value> {
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
                let saved_env = mem::replace(&mut self.env, multi.env.fork());
                self.env.push_scope();

                for (name, val) in all_bindings {
                    self.env.define(&name, val, false);
                }

                if multi.recursion == RecursionMode::SelfBind {
                    self.env
                        .define(&multi.name, Value::MultiClause(Rc::new((*multi).clone())), false);
                }

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
                    result.push(GroupedDecl::Single(Box::new(decl.clone())));
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
                            let mut all_clauses = multi.clauses.clone();
                            all_clauses.extend(clauses.clone());
                            all_clauses
                        }
                        Value::Closure(closure) => {
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
                    recursion: RecursionMode::SelfBind,
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
                    TypeBody::Record(_) => {
                        // Record types don't need special constructor registration
                    }
                    TypeBody::Alias(_) => {
                        // Type aliases are purely a type-level concern; no runtime effect
                    }
                }
                Ok(Value::Unit)
            }

            Decl::Trait { name, items, .. } => {
                self.register_trait(&name.name, items);
                Ok(Value::Unit)
            }
            Decl::Impl {
                trait_name,
                for_type,
                methods,
                ..
            } => {
                self.register_impl(&trait_name.name, &for_type.node, methods);
                Ok(Value::Unit)
            }

            Decl::Let {
                is_mut, pattern, value, ..
            } => {
                let val = self.eval_expr(value)?;
                self.bind_pattern(&pattern.node, &val, *is_mut, pattern.span)?;
                Ok(val)
            }

            Decl::Fn(fn_decl) => {
                match &fn_decl.body {
                    FnBody::Expr(e) => {
                        let closure = Closure {
                            params: fn_decl.params.clone(),
                            body: e.clone(),
                            env: self.env.fork(),
                            name: Some(fn_decl.name.name.clone()),
                        };
                        self.env
                            .define(&fn_decl.name.name, Value::Closure(Rc::new(closure)), false);
                    }
                    FnBody::Guards(_) => {
                        let clause = FnClause {
                            params: fn_decl.params.clone(),
                            body: fn_decl.body.clone(),
                        };
                        let multi = MultiClauseFn {
                            name: fn_decl.name.name.clone(),
                            clauses: vec![clause],
                            env: self.env.fork(),
                            recursion: RecursionMode::SelfBind,
                        };
                        self.env
                            .define(&fn_decl.name.name, Value::MultiClause(Rc::new(multi)), false);
                    }
                }
                Ok(Value::Unit)
            }

            // Type signatures (were) handled by the type checker, noop in runtime
            Decl::TypeSig { .. } => Ok(Value::Unit),
        }
    }
}

enum GroupedDecl {
    Single(Box<Spanned<Decl>>),
    MultiClauseFn { name: String, clauses: Vec<FnClause> },
}

fn group_fn_clauses<'a>(decls: impl IntoIterator<Item = &'a FnDecl>) -> HashMap<String, Vec<FnClause>> {
    let mut grouped: HashMap<String, Vec<FnClause>> = HashMap::new();
    for fn_decl in decls {
        let clause = FnClause {
            params: fn_decl.params.clone(),
            body: fn_decl.body.clone(),
        };
        grouped.entry(fn_decl.name.name.clone()).or_default().push(clause);
    }
    grouped
}
