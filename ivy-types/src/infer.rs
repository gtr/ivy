//! Type inference using Algorithm W (Hindley-Milner).
//!
//! This module implements constraint-based type inference:
//! 1. Walk the AST, generating type variables and constraints
//! 2. Solve constraints via unification
//! 3. Apply the resulting substitution to get final types
//!
//! Reference: <https://smunix.github.io/dev.stephendiehl.com/fun/006_hindley_milner.html>
use crate::env::{TypeEnv, TypeVarGen, BUILTIN_VAR_OFFSET};
use crate::error::{TypeError, TypeResult};
use crate::exhaustiveness;
use crate::registry::{ImplInfo, RecordFieldInfo, TraitInfo, TypeRegistry};
use crate::subst::Subst;
use crate::types::{Scheme, TraitConstraint, Type, TypeVar};
use crate::unify::unify_with_subst;
use ivy_syntax::{
    expr::{Expr, MatchArm, Param},
    lit::Literal,
    op::{BinOp, UnaryOp},
    pattern::Pattern,
    types::TypeExpr,
    Span, Spanned,
};
use std::collections::{HashMap, HashSet};

pub struct TypeChecker {
    gen: TypeVarGen,
    pub subst: Subst,
    pub registry: TypeRegistry,
    pub loaded_modules: HashSet<String>,
    /// Pending trait constraints (with introduction span), partitioned at decl boundaries
    pub constraints: Vec<(TraitConstraint, Span)>,
    /// Constraints assumed in the current context (where-clauses, signatures)
    pub assumed_constraints: Vec<TraitConstraint>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> TypeChecker {
        let mut checker = TypeChecker {
            gen: TypeVarGen::new(),
            subst: Subst::new(),
            registry: TypeRegistry::with_builtins(),
            loaded_modules: HashSet::new(),
            constraints: Vec::new(),
            assumed_constraints: Vec::new(),
        };
        checker.register_builtin_traits();
        checker
    }

    /// Auto-register internal traits like `Num` and `Concat`
    ///
    /// NOTE: these aren't visible to users (no method bindings introduced) but are used by operator constraint
    /// emission:
    ///
    /// `+`, `-`, `*`, `/`, `%`, and unary `-` emit a `Num` constraint instead of defaulting an unresolved
    /// type to `Int`
    ///
    /// `++` emits a `Concat` constraint instead of defaulting an unresolved type to a list
    fn register_builtin_traits(&mut self) {
        let num_param = TypeVar(BUILTIN_VAR_OFFSET);
        self.registry.register_trait(TraitInfo {
            name: "Num".to_string(),
            param: num_param,
            methods: HashMap::new(),
            default_impls: HashMap::new(),
            span: Span::default(),
        });
        for ty in [Type::Int, Type::Float] {
            self.registry.register_impl(ImplInfo {
                trait_name: "Num".to_string(),
                head: ty,
                head_vars: vec![],
                where_constraints: vec![],
                span: Span::default(),
            });
        }

        self.registry.register_trait(TraitInfo {
            name: "Concat".to_string(),
            param: TypeVar(BUILTIN_VAR_OFFSET + 1),
            methods: HashMap::new(),
            default_impls: HashMap::new(),
            span: Span::default(),
        });
        self.registry.register_impl(ImplInfo {
            trait_name: "Concat".to_string(),
            head: Type::String,
            head_vars: vec![],
            where_constraints: vec![],
            span: Span::default(),
        });
        let elem = TypeVar(BUILTIN_VAR_OFFSET + 2);
        self.registry.register_impl(ImplInfo {
            trait_name: "Concat".to_string(),
            head: Type::list(Type::Var(elem)),
            head_vars: vec![elem],
            where_constraints: vec![],
            span: Span::default(),
        });
    }

    /// Infer the type of an expression.
    pub fn infer(&mut self, expr: &Spanned<Expr>, env: &TypeEnv) -> TypeResult<Type> {
        let span = expr.span;
        match &expr.node {
            Expr::Lit(lit) => Ok(self.infer_lit(lit)),

            Expr::Var(ident) => {
                let name = &ident.name;
                match env.get(name) {
                    Some(scheme) => {
                        let scheme = scheme.clone();
                        Ok(self.instantiate(&scheme, span))
                    }
                    None => Err(TypeError::undefined_variable(name, span)),
                }
            }

            Expr::Binary { left, op, right } => self.infer_binary(left, op.node, right, env, span),

            Expr::Unary { op, operand } => self.infer_unary(op.node, operand, env, span),

            Expr::Let { pattern, ty, value, .. } => self.infer_let(pattern, ty.as_deref(), value, env),

            Expr::Assign { target, value } => self.infer_assign(target, value, env, span),

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.infer_if(condition, then_branch, else_branch, env, span),

            Expr::Match { scrutinee, arms } => self.infer_match(scrutinee, arms, env),

            Expr::Lambda {
                params,
                return_ty,
                body,
            } => self.infer_lambda(params, return_ty.as_deref(), body, env),

            Expr::Call { callee, args } => self.infer_call(callee, args, env, span),

            Expr::Field { object, field } => self.infer_field(object, &field.name, env, span),

            Expr::Index { object, index } => self.infer_index(object, index, env, span),

            Expr::Do { body } => self.infer_do(body, env),

            Expr::Tuple { elements } => self.infer_tuple(elements, env),

            Expr::List { elements } => self.infer_list(elements, env, span),

            Expr::Record { name, fields } => {
                // look up the record type definition from the registry
                let expected_fields_opt = self
                    .registry
                    .get_record_fields(&name.name)
                    .map(<[RecordFieldInfo]>::to_vec);

                if let Some(expected_fields) = expected_fields_opt {
                    if fields.len() != expected_fields.len() {
                        return Err(TypeError::record_field_count(
                            &name.name,
                            expected_fields.len(),
                            fields.len(),
                            span,
                        ));
                    }
                    for field in fields {
                        let field_ty = self.infer(&field.value, env)?;

                        // Find the expected type for this field
                        if let Some(expected) = expected_fields.iter().find(|f| f.name == field.name.name) {
                            // unify with expected type (expected first for correct error messages)
                            unify_with_subst(&expected.ty, &field_ty, &mut self.subst, field.span)?;
                        } else {
                            return Err(TypeError::undefined_field(&name.name, &field.name.name, field.span));
                        }
                    }
                    for expected in &expected_fields {
                        if !fields.iter().any(|f| f.name.name == expected.name) {
                            return Err(TypeError::missing_field(&name.name, &expected.name, span));
                        }
                    }

                    Ok(Type::named(&name.name))
                } else {
                    // Fallback to structural typing if type not found, this handles cases where record type wasnt declared
                    let mut field_types = Vec::new();
                    for field in fields {
                        let ty = self.infer(&field.value, env)?;
                        field_types.push((field.name.name.clone(), ty));
                    }
                    Ok(Type::Record(name.name.clone(), field_types))
                }
            }

            Expr::RecordUpdate { base, updates } => {
                let base_ty = self.infer(base, env)?;

                // Base must be a record type
                match self.subst.apply(&base_ty) {
                    Type::Record(name, mut fields) => {
                        for update in updates {
                            let update_ty = self.infer(&update.value, env)?;
                            let field_name = &update.name.name;

                            if let Some((_, ty)) = fields.iter_mut().find(|(n, _)| n == field_name) {
                                unify_with_subst(ty, &update_ty, &mut self.subst, update.span)?;
                                *ty = update_ty;
                            } else {
                                return Err(TypeError::undefined_field(&name, field_name, update.span));
                            }
                        }
                        Ok(Type::Record(name, fields))
                    }
                    ty => Err(TypeError::not_a_record(ty, base.span)),
                }
            }

            Expr::Paren { inner } => self.infer(inner, env),
        }
    }

    /// Infer the type of a literal.
    fn infer_lit(&self, lit: &Literal) -> Type {
        match lit {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::String(_) => Type::String,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Bool,
            Literal::Unit => Type::Unit,
        }
    }

    /// Infer the type of a binary expression.
    fn infer_binary(
        &mut self,
        left: &Spanned<Expr>,
        op: BinOp,
        right: &Spanned<Expr>,
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
        let left_ty = self.infer(left, env)?;
        let right_ty = self.infer(right, env)?;

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                unify_with_subst(&left_ty, &right_ty, &mut self.subst, span)?;
                self.infer_numeric_op(&left_ty, span)
            }

            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                unify_with_subst(&left_ty, &right_ty, &mut self.subst, span)?;
                Ok(Type::Bool)
            }

            BinOp::And | BinOp::Or => {
                unify_with_subst(&left_ty, &Type::Bool, &mut self.subst, left.span)?;
                unify_with_subst(&right_ty, &Type::Bool, &mut self.subst, right.span)?;
                Ok(Type::Bool)
            }

            BinOp::Cons => {
                let elem_ty = left_ty;
                let list_ty = Type::list(elem_ty.clone());
                unify_with_subst(&right_ty, &list_ty, &mut self.subst, right.span)?;
                Ok(list_ty)
            }

            BinOp::Concat => {
                unify_with_subst(&left_ty, &right_ty, &mut self.subst, span)?;
                let resolved = self.subst.apply(&left_ty);
                self.infer_concat_op(&resolved, span)
            }
        }
    }

    /// Infer the type of a unary expression.
    fn infer_unary(&mut self, op: UnaryOp, operand: &Spanned<Expr>, env: &TypeEnv, span: Span) -> TypeResult<Type> {
        let operand_ty = self.infer(operand, env)?;

        match op {
            // Negation: Int -> Int or Float -> Float
            UnaryOp::Neg => self.infer_numeric_op(&operand_ty, span),
            // Not: Bool -> Bool
            UnaryOp::Not => {
                unify_with_subst(&operand_ty, &Type::Bool, &mut self.subst, operand.span)?;
                Ok(Type::Bool)
            }
        }
    }

    fn infer_let(
        &mut self,
        pattern: &Spanned<Pattern>,
        ty_ann: Option<&Spanned<TypeExpr>>,
        value: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> TypeResult<Type> {
        let value_ty = self.infer(value, env)?;

        if let Some(ann) = ty_ann {
            let mut scope = HashMap::new();
            let ann_ty = self.type_expr_to_type_scoped(&ann.node, env, Some(&mut scope));
            unify_with_subst(&ann_ty, &value_ty, &mut self.subst, value.span)
                .map_err(|e| crate::add_expected_span(e, ann.span))?;
        }

        match &pattern.node {
            Pattern::Var(_) => Ok(Type::Unit), // Let returns ()
            Pattern::Wildcard => Ok(Type::Unit),
            _ => {
                self.check_pattern(pattern, &value_ty, env)?;
                Ok(Type::Unit)
            }
        }
    }

    fn infer_assign(
        &mut self,
        target: &Spanned<Expr>,
        value: &Spanned<Expr>,
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
        let target_ty = self.infer(target, env)?;
        let value_ty = self.infer(value, env)?;
        unify_with_subst(&target_ty, &value_ty, &mut self.subst, span)?;
        Ok(Type::Unit)
    }

    fn infer_if(
        &mut self,
        condition: &Spanned<Expr>,
        then_branch: &Spanned<Expr>,
        else_branch: &Spanned<Expr>,
        env: &TypeEnv,
        _span: Span,
    ) -> TypeResult<Type> {
        let cond_ty = self.infer(condition, env)?;
        unify_with_subst(&Type::Bool, &cond_ty, &mut self.subst, condition.span)?;

        let then_ty = self.infer(then_branch, env)?;
        let else_ty = self.infer(else_branch, env)?;
        // expected = then-branch type (the "first one wins"), found = else-branch type.
        unify_with_subst(&then_ty, &else_ty, &mut self.subst, else_branch.span)
            .map_err(|e| crate::add_expected_span(e, then_branch.span))?;

        Ok(then_ty)
    }

    fn infer_match(&mut self, scrutinee: &Spanned<Expr>, arms: &[MatchArm], env: &TypeEnv) -> TypeResult<Type> {
        let scrutinee_ty = self.infer(scrutinee, env)?;

        if arms.is_empty() {
            return Ok(self.gen.fresh_type());
        }

        let mut result_ty: Option<Type> = None;

        for arm in arms {
            let bindings = self.infer_pattern(&arm.pattern, &scrutinee_ty, env)?;
            let arm_env = env.extend(bindings);
            let body_ty = self.infer(&arm.body, &arm_env)?;

            match &result_ty {
                Some(ty) => {
                    unify_with_subst(ty, &body_ty, &mut self.subst, arm.span)?;
                }
                None => {
                    result_ty = Some(body_ty);
                }
            }
        }
        let patterns: Vec<&Pattern> = arms.iter().map(|arm| &arm.pattern.node).collect();
        let resolved_ty = self.subst.apply(&scrutinee_ty);
        exhaustiveness::check_exhaustiveness(&resolved_ty, &patterns, &self.registry, scrutinee.span)?;

        Ok(result_ty.unwrap_or_else(|| self.gen.fresh_type()))
    }

    fn infer_lambda(
        &mut self,
        params: &[Param],
        return_ty: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> TypeResult<Type> {
        let mut param_types = Vec::new();
        let mut bindings = Vec::new();
        let mut type_var_scope: HashMap<String, Type> = HashMap::new();

        for param in params {
            let ty = if let Some(ann) = &param.ty {
                self.type_expr_to_type_scoped(&ann.node, env, Some(&mut type_var_scope))
            } else {
                self.gen.fresh_type()
            };

            if let Pattern::Var(ident) = &param.pattern.node {
                bindings.push((ident.name.clone(), Scheme::mono(ty.clone())));
            }

            param_types.push(ty);
        }

        let body_env = env.extend(bindings);
        let body_ty = self.infer(body, &body_env)?;

        if let Some(ann) = return_ty {
            let ann_ty = self.type_expr_to_type_scoped(&ann.node, env, Some(&mut type_var_scope));
            unify_with_subst(&ann_ty, &body_ty, &mut self.subst, body.span)
                .map_err(|e| crate::add_expected_span(e, ann.span))?;
        }

        let mut result = body_ty;
        for param_ty in param_types.into_iter().rev() {
            result = Type::fun(param_ty, result);
        }

        Ok(result)
    }

    fn infer_call(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<Expr>],
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
        let callee_ty = self.infer(callee, env)?;
        let resolved_callee = self.subst.apply(&callee_ty);

        // Compute the callee's arity if it's a fully-known function chain
        // Returns None if the type is a type variable (unknown arity) or non-function.
        let callee_arity = arity_of(&resolved_callee);

        // Walk param-by-param: each arg unifies against the matching param type
        // The arg's own span becomes the primary error location.
        let mut current = resolved_callee;
        for (i, arg) in args.iter().enumerate() {
            let arg_ty = self.infer(arg, env)?;
            let resolved = self.subst.apply(&current);
            match resolved {
                Type::Fun(param, result) => {
                    unify_with_subst(&param, &arg_ty, &mut self.subst, arg.span)
                        .map_err(|e| crate::add_expected_span(e, callee.span))?;
                    current = *result;
                }
                _ => {
                    // Ran out of params. If we know the callee's arity, this is an
                    // arity mismatch. Otherwise we fall back to general unification.
                    if let Some(arity) = callee_arity {
                        let name = callee_name(callee);
                        return Err(TypeError::arity_mismatch(&name, arity, args.len(), span));
                    }
                    let result_ty = self.gen.fresh_type();
                    let expected = Type::fun(arg_ty, result_ty.clone());
                    unify_with_subst(&resolved, &expected, &mut self.subst, args[i].span)?;
                    current = result_ty;
                }
            }
        }

        Ok(current)
    }

    /// Infer the type of a field access
    fn infer_field(&mut self, object: &Spanned<Expr>, field: &str, env: &TypeEnv, span: Span) -> TypeResult<Type> {
        if let Expr::Var(ident) = &object.node {
            let module_name = &ident.name;
            if let Some(scheme) = env.get_module_export(module_name, field) {
                let scheme = scheme.clone();
                return Ok(self.instantiate(&scheme, span));
            }
            if env.is_module(module_name) {
                return Err(TypeError::undefined_field(module_name, field, span));
            }
        }

        let obj_ty = self.infer(object, env)?;
        let resolved = self.subst.apply(&obj_ty);

        match resolved {
            Type::Record(name, fields) => {
                for (field_name, field_ty) in &fields {
                    if field_name == field {
                        return Ok(field_ty.clone());
                    }
                }
                Err(TypeError::undefined_field(&name, field, span))
            }
            Type::Tuple(elems) => {
                if let Ok(idx) = field.parse::<usize>() {
                    if idx < elems.len() {
                        return Ok(elems[idx].clone());
                    }
                }
                Err(TypeError::undefined_field("tuple", field, span))
            }
            ty => Err(TypeError::not_a_record(ty, span)),
        }
    }

    /// Infer the type of an index access.
    fn infer_index(
        &mut self,
        object: &Spanned<Expr>,
        index: &Spanned<Expr>,
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
        let obj_ty = self.infer(object, env)?;
        let idx_ty = self.infer(index, env)?;

        unify_with_subst(&idx_ty, &Type::Int, &mut self.subst, index.span)?;

        let resolved = self.subst.apply(&obj_ty);
        match resolved {
            Type::List(elem) => Ok(*elem),
            Type::Tuple(elems) => {
                // TODO(gtr): For tuples, index is known at compile time
                // for now, return a fresh type (runtime check)
                if elems.is_empty() {
                    Ok(self.gen.fresh_type())
                } else {
                    // TODO(gtr): We can't know which element, so return fresh
                    Ok(self.gen.fresh_type())
                }
            }
            Type::String => Ok(Type::Char),
            ty => Err(TypeError::not_indexable(ty, span)),
        }
    }

    /// Infer the type of a do block.
    fn infer_do(&mut self, body: &[Spanned<Expr>], env: &TypeEnv) -> TypeResult<Type> {
        if body.is_empty() {
            return Ok(Type::Unit);
        }

        let mut current_env = env.clone();
        let mut last_ty = Type::Unit;

        for expr in body {
            match &expr.node {
                Expr::Let { pattern, value, ty, .. } => {
                    let value_ty = self.infer(value, &current_env)?;
                    if let Some(ann) = ty {
                        let mut scope = HashMap::new();
                        let ann_ty = self.type_expr_to_type_scoped(&ann.node, &current_env, Some(&mut scope));
                        unify_with_subst(&ann_ty, &value_ty, &mut self.subst, value.span)
                            .map_err(|e| crate::add_expected_span(e, ann.span))?;
                    }

                    let bindings = self.infer_pattern(pattern, &value_ty, &current_env)?;
                    for (name, scheme) in bindings {
                        current_env.insert(name, scheme);
                    }
                    last_ty = Type::Unit;
                }
                _ => {
                    last_ty = self.infer(expr, &current_env)?;
                }
            }
        }

        Ok(last_ty)
    }

    /// Infer the type of a tuple.
    fn infer_tuple(&mut self, elements: &[Spanned<Expr>], env: &TypeEnv) -> TypeResult<Type> {
        let mut elem_types = Vec::new();
        for elem in elements {
            elem_types.push(self.infer(elem, env)?);
        }
        Ok(Type::Tuple(elem_types))
    }

    /// Infer the type of a list.
    fn infer_list(&mut self, elements: &[Spanned<Expr>], env: &TypeEnv, _span: Span) -> TypeResult<Type> {
        if elements.is_empty() {
            return Ok(Type::list(self.gen.fresh_type()));
        }

        let first_ty = self.infer(&elements[0], env)?;
        let first_span = elements[0].span;

        for elem in &elements[1..] {
            let elem_ty = self.infer(elem, env)?;
            unify_with_subst(&first_ty, &elem_ty, &mut self.subst, elem.span)
                .map_err(|e| crate::add_expected_span(e, first_span))?;
        }

        Ok(Type::list(first_ty))
    }

    /// Infer pattern bindings and check it matches the expected type.
    pub fn infer_pattern(
        &mut self,
        pattern: &Spanned<Pattern>,
        expected: &Type,
        env: &TypeEnv,
    ) -> TypeResult<Vec<(String, Scheme)>> {
        let mut bindings = Vec::new();
        self.collect_pattern_bindings(pattern, expected, &mut bindings, env)?;
        Ok(bindings)
    }

    fn collect_pattern_bindings(
        &mut self,
        pattern: &Spanned<Pattern>,
        expected: &Type,
        bindings: &mut Vec<(String, Scheme)>,
        env: &TypeEnv,
    ) -> TypeResult<()> {
        let span = pattern.span;
        let expected = self.subst.apply(expected);

        match &pattern.node {
            Pattern::Wildcard => Ok(()),

            Pattern::Var(ident) => {
                bindings.push((ident.name.clone(), Scheme::mono(expected)));
                Ok(())
            }

            Pattern::Lit(lit) => {
                let lit_ty = self.infer_lit(lit);
                unify_with_subst(&expected, &lit_ty, &mut self.subst, span)?;
                Ok(())
            }

            Pattern::Tuple { elements } => match expected {
                Type::Tuple(tys) if tys.len() == elements.len() => {
                    for (pat, ty) in elements.iter().zip(tys.iter()) {
                        self.collect_pattern_bindings(pat, ty, bindings, env)?;
                    }
                    Ok(())
                }
                Type::Var(_) => {
                    let tys: Vec<Type> = elements.iter().map(|_| self.gen.fresh_type()).collect();
                    unify_with_subst(&expected, &Type::Tuple(tys.clone()), &mut self.subst, span)?;
                    for (pat, ty) in elements.iter().zip(tys.iter()) {
                        self.collect_pattern_bindings(pat, ty, bindings, env)?;
                    }
                    Ok(())
                }
                _ => Err(TypeError::mismatch(Type::Tuple(vec![]), expected.clone(), span)),
            },

            Pattern::List { elements } => {
                // Special case: [head :: tail] is parsed as List containing Cons,
                // but _semantically_ it's just a cons pattern
                if elements.len() == 1 {
                    if let Pattern::Cons { head, tail } = &elements[0].node {
                        let elem_ty = self.gen.fresh_type();
                        let list_ty = Type::list(elem_ty.clone());

                        unify_with_subst(&expected, &list_ty, &mut self.subst, span)?;
                        self.collect_pattern_bindings(head, &elem_ty, bindings, env)?;
                        self.collect_pattern_bindings(tail, &list_ty, bindings, env)?;
                        return Ok(());
                    }
                }

                let elem_ty = self.gen.fresh_type();
                unify_with_subst(&expected, &Type::list(elem_ty.clone()), &mut self.subst, span)?;
                for pat in elements {
                    self.collect_pattern_bindings(pat, &elem_ty, bindings, env)?;
                }
                Ok(())
            }

            Pattern::Cons { head, tail } => {
                let elem_ty = self.gen.fresh_type();
                let list_ty = Type::list(elem_ty.clone());
                unify_with_subst(&expected, &list_ty, &mut self.subst, span)?;
                self.collect_pattern_bindings(head, &elem_ty, bindings, env)?;
                self.collect_pattern_bindings(tail, &list_ty, bindings, env)?;
                Ok(())
            }

            Pattern::Constructor { name, args } => {
                // Look up constructor in environment
                if let Some(scheme) = env.get(&name.name) {
                    let scheme = scheme.clone();
                    let ctor_ty = self.instantiate(&scheme, span);

                    // The constructor should be a function type (for constructors with args)
                    // or a plain type (for nullary constructors like None)
                    if args.is_empty() {
                        unify_with_subst(&expected, &ctor_ty, &mut self.subst, span)?;
                    } else {
                        let mut current_ty = ctor_ty;
                        let mut arg_types = Vec::new();

                        for _ in 0..args.len() {
                            match current_ty {
                                Type::Fun(arg_ty, ret_ty) => {
                                    arg_types.push(self.subst.apply(&arg_ty));
                                    current_ty = self.subst.apply(&ret_ty);
                                }
                                _ => {
                                    return Err(TypeError::arity_mismatch(
                                        &name.name,
                                        arg_types.len(),
                                        args.len(),
                                        span,
                                    ));
                                }
                            }
                        }

                        unify_with_subst(&expected, &current_ty, &mut self.subst, span)?;
                        for (arg, arg_ty) in args.iter().zip(arg_types.iter()) {
                            self.collect_pattern_bindings(arg, arg_ty, bindings, env)?;
                        }
                    }
                } else {
                    for arg in args {
                        let arg_ty = self.gen.fresh_type();
                        self.collect_pattern_bindings(arg, &arg_ty, bindings, env)?;
                    }
                }
                Ok(())
            }

            Pattern::Record { name, fields } => {
                // look up the record in the registry so each field pattern is
                // checked against the actual declared field type
                let record_info = self.registry.get_record(&name.name).cloned();
                if let Some(info) = record_info {
                    // allocate fresh vars for the record's type parameters and
                    // build a substitution to instantiate field types
                    let fresh_params: Vec<Type> = info.params.iter().map(|_| self.gen.fresh_type()).collect();
                    let mut param_subst: HashMap<TypeVar, Type> = HashMap::new();
                    for (p, fresh) in info.params.iter().zip(fresh_params.iter()) {
                        param_subst.insert(*p, fresh.clone());
                    }

                    // Unify the scrutinee with the named record type.
                    let head_ty = if fresh_params.is_empty() {
                        Type::named(&name.name)
                    } else {
                        Type::named_with(&name.name, fresh_params.clone())
                    };
                    unify_with_subst(&head_ty, &expected, &mut self.subst, span)?;

                    for field in fields {
                        let declared = info
                            .fields
                            .iter()
                            .find(|f| f.name == field.name.name)
                            .ok_or_else(|| TypeError::undefined_field(&name.name, &field.name.name, field.span))?;
                        let field_ty = substitute_type(&declared.ty, &param_subst);
                        if let Some(pat) = &field.pattern {
                            self.collect_pattern_bindings(pat, &field_ty, bindings, env)?;
                        } else {
                            bindings.push((field.name.name.clone(), Scheme::mono(field_ty)));
                        }
                    }
                    Ok(())
                } else {
                    for field in fields {
                        let field_ty = self.gen.fresh_type();
                        if let Some(pat) = &field.pattern {
                            self.collect_pattern_bindings(pat, &field_ty, bindings, env)?;
                        } else {
                            bindings.push((field.name.name.clone(), Scheme::mono(field_ty)));
                        }
                    }
                    Ok(())
                }
            }

            Pattern::Or { left, right } => {
                // Both sides must bind the same names with unifiable types
                let mut left_bindings = Vec::new();
                let mut right_bindings = Vec::new();
                self.collect_pattern_bindings(left, &expected, &mut left_bindings, env)?;
                self.collect_pattern_bindings(right, &expected, &mut right_bindings, env)?;

                let left_names: HashSet<&str> = left_bindings.iter().map(|(n, _)| n.as_str()).collect();
                let right_names: HashSet<&str> = right_bindings.iter().map(|(n, _)| n.as_str()).collect();
                if left_names != right_names {
                    return Err(TypeError::OrPatternBindingMismatch { span });
                }

                for (lname, lscheme) in &left_bindings {
                    let (_, rscheme) = right_bindings
                        .iter()
                        .find(|(rname, _)| rname == lname)
                        .expect("name-set equality just verified");
                    unify_with_subst(&lscheme.ty, &rscheme.ty, &mut self.subst, span)?;
                }
                bindings.extend(left_bindings);
                Ok(())
            }
        }
    }

    /// Check a pattern against an expected type (without collecting bindings).
    fn check_pattern(&mut self, pattern: &Spanned<Pattern>, expected: &Type, env: &TypeEnv) -> TypeResult<()> {
        let mut bindings = Vec::new();
        self.collect_pattern_bindings(pattern, expected, &mut bindings, env)
    }

    /// Emit a `Num` constraint on `ty` so that `fn double(x) => x + x` generalizes
    /// to `forall a. Num a => a -> a` instead of defaulting `a` to `Int`
    fn infer_numeric_op(&mut self, ty: &Type, span: Span) -> TypeResult<Type> {
        let resolved = self.subst.apply(ty);
        match &resolved {
            Type::Int | Type::Float | Type::Var(_) => {
                self.constraints.push((
                    TraitConstraint {
                        trait_name: "Num".to_string(),
                        type_args: vec![resolved.clone()],
                    },
                    span,
                ));
                Ok(resolved)
            }
            _ => Err(TypeError::mismatch(Type::Int, resolved, span)),
        }
    }

    /// An unresolved operand emits a `Concat` constraint and stays polymorphic rather than defaulting to a list so
    /// `fn f(x, y) => x ++ y` also types at `String`
    /// TODO(gtr): Might be a cleaner way to do this later
    fn infer_concat_op(&mut self, ty: &Type, span: Span) -> TypeResult<Type> {
        let resolved = self.subst.apply(ty);
        match &resolved {
            Type::String | Type::List(_) | Type::Var(_) => {
                self.constraints.push((
                    TraitConstraint {
                        trait_name: "Concat".to_string(),
                        type_args: vec![resolved.clone()],
                    },
                    span,
                ));
                Ok(resolved)
            }
            _ => Err(TypeError::mismatch(Type::String, resolved, span)),
        }
    }

    pub fn fresh_type(&mut self) -> Type {
        self.gen.fresh_type()
    }

    pub fn fresh_var(&mut self) -> TypeVar {
        self.gen.fresh()
    }

    pub fn instantiate(&mut self, scheme: &Scheme, span: Span) -> Type {
        let (ty, cs) = self.gen.instantiate(scheme);
        for c in cs {
            self.constraints.push((c, span));
        }
        ty
    }

    /// If `name` is a registered alias, expand it by substituting `args` for the
    /// alias's type parameters. If no args are supplied, fresh type vars are used.
    fn expand_alias_if_present(&mut self, name: &str, args: &[Type]) -> Option<Type> {
        let info = self.registry.get_alias(name)?.clone();
        let mapping: HashMap<TypeVar, Type> = if args.is_empty() {
            // No args supplied: instantiate with fresh type vars.
            info.params.iter().map(|&v| (v, self.gen.fresh_type())).collect()
        } else if args.len() == info.params.len() {
            info.params.iter().copied().zip(args.iter().cloned()).collect()
        } else {
            return None;
        };
        Some(substitute_type(&info.body, &mapping))
    }

    pub fn type_expr_to_type(&mut self, ty_expr: &TypeExpr, env: &TypeEnv) -> Type {
        self.type_expr_to_type_scoped(ty_expr, env, None)
    }

    pub fn type_expr_to_type_scoped(
        &mut self,
        ty_expr: &TypeExpr,
        env: &TypeEnv,
        mut scope: Option<&mut HashMap<String, Type>>,
    ) -> Type {
        match ty_expr {
            TypeExpr::Named(ident) => {
                let name = &ident.name;
                match name.as_str() {
                    "Int" => Type::Int,
                    "Float" => Type::Float,
                    "Bool" => Type::Bool,
                    "String" => Type::String,
                    "Char" => Type::Char,
                    _ => {
                        if let Some(expanded) = self.expand_alias_if_present(name, &[]) {
                            expanded
                        } else if self.registry.is_sum_type(name) || self.registry.is_record_type(name) {
                            // Registered user-defined type. Construct the named type directly,
                            // even if a constructor of the same name exists in `env`.
                            Type::Named(name.clone(), vec![])
                        } else if let Some(scheme) = env.get(name).cloned() {
                            self.instantiate(&scheme, ident.span)
                        } else if let Some(ref mut sc) = scope {
                            // If we have a scope and name looks like a type variable use consistent type variables
                            if Self::looks_like_type_var(name) {
                                if let Some(ty) = sc.get(name) {
                                    ty.clone()
                                } else {
                                    let fresh = self.gen.fresh_type();
                                    sc.insert(name.clone(), fresh.clone());
                                    fresh
                                }
                            } else {
                                // Unknown type, create a named type (for forward references)
                                Type::Named(name.clone(), vec![])
                            }
                        } else {
                            // No scope, just create a named type
                            Type::Named(name.clone(), vec![])
                        }
                    }
                }
            }
            TypeExpr::Apply { base, args } => {
                let name = &base.name;
                let mut type_args = Vec::new();
                for a in args {
                    type_args.push(self.type_expr_to_type_scoped(&a.node, env, scope.as_deref_mut()));
                }
                if let Some(expanded) = self.expand_alias_if_present(name, &type_args) {
                    expanded
                } else {
                    Type::Named(name.clone(), type_args)
                }
            }
            TypeExpr::Function { param, result } => {
                let param_ty = self.type_expr_to_type_scoped(&param.node, env, scope.as_deref_mut());
                let result_ty = self.type_expr_to_type_scoped(&result.node, env, scope.as_deref_mut());
                Type::fun(param_ty, result_ty)
            }
            TypeExpr::Tuple { elements } => {
                let mut tys = Vec::new();
                for e in elements {
                    tys.push(self.type_expr_to_type_scoped(&e.node, env, scope.as_deref_mut()));
                }
                Type::Tuple(tys)
            }
            TypeExpr::List { element } => {
                Type::list(self.type_expr_to_type_scoped(&element.node, env, scope.as_deref_mut()))
            }
            TypeExpr::Unit => Type::Unit,
            TypeExpr::Var(ident) => {
                // Lowercase type variable in annotation
                if let Some(ref mut sc) = scope {
                    if let Some(ty) = sc.get(&ident.name) {
                        ty.clone()
                    } else {
                        let fresh = self.gen.fresh_type();
                        sc.insert(ident.name.clone(), fresh.clone());
                        fresh
                    }
                } else {
                    self.gen.fresh_type()
                }
            }
        }
    }

    fn looks_like_type_var(name: &str) -> bool {
        name.len() <= 3 && name.chars().all(char::is_uppercase)
    }

    pub fn finalize(&self, ty: &Type) -> Type {
        self.subst.apply(ty)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        TypeChecker::new()
    }
}

fn substitute_type(ty: &Type, mapping: &HashMap<TypeVar, Type>) -> Type {
    Subst::from_mappings(mapping.clone()).apply(ty)
}

/// Count parameters in a function type chain. Returns None if the head is
/// not a `Type::Fun` (the type is a type variable or other non-function).
fn arity_of(ty: &Type) -> Option<usize> {
    let mut n = 0;
    let mut current = ty;
    while let Type::Fun(_, result) = current {
        n += 1;
        current = result;
    }
    if n == 0 {
        None
    } else {
        Some(n)
    }
}

/// Best-effort name for the callee used in arity error messages.
fn callee_name(callee: &Spanned<Expr>) -> String {
    match &callee.node {
        Expr::Var(ident) => ident.name.clone(),
        Expr::Field { field, .. } => field.name.clone(),
        _ => "function".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ivy_syntax::decl::Decl;
    use ivy_syntax::Spanned;

    fn check(code: &str) -> TypeResult<Type> {
        let program = ivy_parse::parse(code).expect("parse failed");
        let mut env = TypeEnv::with_builtins();
        let mut checker = TypeChecker::new();

        let mut result = Type::Unit;
        for decl in &program.declarations {
            result = infer_decl(&mut checker, decl, &mut env)?;
        }
        Ok(checker.finalize(&result))
    }

    fn infer_decl(checker: &mut TypeChecker, decl: &Spanned<Decl>, env: &mut TypeEnv) -> TypeResult<Type> {
        match &decl.node {
            Decl::Let { pattern, value, .. } => {
                let value_ty = checker.infer(value, env)?;
                // Bind the pattern variable to the type
                if let Pattern::Var(ident) = &pattern.node {
                    let scheme = env.generalize(&checker.finalize(&value_ty));
                    env.insert(ident.name.clone(), scheme);
                }
                Ok(value_ty)
            }
            Decl::Fn(_) => Ok(Type::Unit), // TODO(gtr): Function declarations need more work...
            _ => Ok(Type::Unit),
        }
    }

    #[test]
    fn test_infer_arithmetic() {
        assert_eq!(check("1 + 2;").unwrap(), Type::Int);
        assert_eq!(check("3.0 * 4.0;").unwrap(), Type::Float);
    }

    #[test]
    fn test_infer_comparison() {
        assert_eq!(check("1 < 2;").unwrap(), Type::Bool);
        assert_eq!(check("1 == 2;").unwrap(), Type::Bool);
    }

    #[test]
    fn test_infer_list() {
        let ty = check("[1, 2, 3];").unwrap();
        assert_eq!(ty, Type::list(Type::Int));
    }

    #[test]
    fn test_infer_tuple() {
        let ty = check("(1, true);").unwrap();
        assert_eq!(ty, Type::Tuple(vec![Type::Int, Type::Bool]));
    }

    #[test]
    fn test_infer_if() {
        assert_eq!(check("if true then 1 else 2;").unwrap(), Type::Int);
    }

    #[test]
    fn test_infer_lambda() {
        // Lambda must be in expression context, not at top level
        let ty = check("let f = fn (x) => x + 1; f;").unwrap();
        assert_eq!(ty, Type::fun(Type::Int, Type::Int));
    }

    #[test]
    fn test_type_error() {
        assert!(check("1 + true;").is_err());
    }

    #[test]
    fn test_if_branch_mismatch() {
        assert!(check("if true then 1 else \"no\";").is_err());
    }

    /// Run the full type-checking pipeline (handles type decls, including aliases).
    fn check_program(code: &str) -> TypeResult<()> {
        let program = ivy_parse::parse(code).expect("parse failed");
        let mut env = TypeEnv::with_builtins();
        let mut checker = TypeChecker::new();
        let mut loader = ivy_parse::ModuleLoader::new(vec![]);
        crate::check_program_with_env(&program, &mut checker, &mut env, &mut loader)
    }

    fn check_module(code: &str) -> TypeResult<()> {
        let program = ivy_parse::parse(code).expect("parse failed");
        let mut checker = TypeChecker::new();
        let mut loader = ivy_parse::ModuleLoader::new(vec![]);
        crate::type_check_module(&program, &mut checker, &mut loader).map(|_| ())
    }

    #[test]
    fn test_alias_simple() {
        assert!(check_program("type Latitude = Float; let x: Latitude = 45.0;").is_ok());
    }

    #[test]
    fn test_alias_mismatch() {
        // String value doesn't match a Float alias.
        assert!(check_program("type Latitude = Float; let x: Latitude = \"hi\";").is_err());
    }

    #[test]
    fn test_alias_compound() {
        let code = "type Env = [(String, Int)]; let e: Env = [(\"x\", 1), (\"y\", 2)];";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_alias_parametric() {
        let code = "type Pair<a, b> = (a, b); let p: Pair<Int, String> = (1, \"hi\");";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_alias_chain() {
        // Aliases of aliases should expand transitively
        let code = "type A = Int; type B = A; let x: B = 42;";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_alias_function_type() {
        let code = "type IntFn = Int -> Int; fn double(x) => x * 2; let f: IntFn = double;";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_newtype_distinct_from_underlying() {
        // Float cannot be assigned to a Latitude position.
        assert!(check_program("newtype Latitude = Float; let lat: Latitude = 45.0;").is_err());
    }

    #[test]
    fn test_newtype_construction() {
        // Wrapping with the constructor works.
        assert!(check_program("newtype Latitude = Float; let lat = Latitude(45.0);").is_ok());
    }

    #[test]
    fn test_newtype_unwrap_via_match() {
        let code = "newtype Latitude = Float; \
                    let lat = Latitude(45.0); \
                    let raw = match lat with | Latitude(f) -> f end;";
        assert!(check_program(code).is_ok());
    }

    // annotation type-var scope is shared
    #[test]
    fn test_lambda_type_var_scope_is_consistent() {
        let code = "let f: a -> a = fn (x) => 5; let r = f(\"hi\");";
        assert!(check_program(code).is_err());
    }

    // record patterns must use the registry
    #[test]
    fn test_record_pattern_uses_registry() {
        let code = "type Person = { name: String, age: Int }; \
                    fn f(p) => match p with | Person { age: a } -> a + \"oops\" end;";
        assert!(check_program(code).is_err());
    }

    // or-patterns must bind matching name sets
    #[test]
    fn test_or_pattern_requires_matching_bindings() {
        let code = "let r = match Some(1) with | Some(x) | None -> x end;";
        assert!(check_program(code).is_err());
    }

    // or-pattern bindings must unify across sides
    #[test]
    fn test_or_pattern_unifies_binding_types() {
        let code = "type Either = | L(Int) | R(String); \
                    fn f(e) => match e with | L(x) | R(x) -> x end;";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_trait_decl_registers_methods() {
        let code = "trait Show<a> { fn show(x: a): String; } \
                    impl Show for Int { fn show(n) => __intToString(n); } \
                    let r = show(42);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_no_impl_found_grounds_error() {
        let code = "trait Show<a> { fn show(x: a): String; } \
                    impl Show for Int { fn show(n) => __intToString(n); } \
                    let r = show(true);";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_constraint_propagation_through_polymorphic_fn() {
        let code = "trait Show<a> { fn show(x: a): String; } \
                    impl Show for Int { fn show(n) => __intToString(n); } \
                    fn greet(x) => show(x); \
                    let r = greet(42);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_constraint_propagation_unknown_impl() {
        let code = "trait Show<a> { fn show(x: a): String; } \
                    impl Show for Int { fn show(n) => __intToString(n); } \
                    fn greet(x) => show(x); \
                    let r = greet(true);";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_unknown_trait_in_impl() {
        let code = "impl NoSuch for Int { fn foo(x) => x; }";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_overlapping_impls_rejected() {
        let code = "trait Show<a> { fn show(x: a): String; } \
                    impl Show for Int { fn show(n) => __intToString(n); } \
                    impl Show for Int { fn show(n) => \"oops\"; }";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_multi_param_trait_rejected() {
        let code = "trait Bad<a, b> { fn x(a: a, b: b): a; }";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_impl_with_where_clause() {
        let code = "trait Show<a> { fn show(x: a): String; } \
                    impl Show for Int { fn show(n) => __intToString(n); } \
                    impl Show for Option<a> where Show<a> { \
                        fn show(None) => \"None\"; \
                        fn show(Some(x)) => __intToString(0); \
                    } \
                    let r = show(Some(5));";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_num_constraint_via_arithmetic_int() {
        let code = "fn double(x) => x + x; let r = double(3);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_num_constraint_via_arithmetic_float() {
        let code = "fn double(x) => x + x; let r = double(3.0);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_num_constraint_rejects_non_numeric() {
        let code = "fn double(x) => x + x; let r = double(\"hi\");";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_blanket_impl_rejected() {
        let code = "trait T<a> { fn t(x: a): Bool; } \
                    impl T for a { fn t(_) => true; }";
        assert!(check_program(code).is_err());
    }

    #[test]
    fn test_impl_method_can_call_sibling_trait_method() {
        let code = "type Foo = | Bar; \
                    trait MyEq<a> { \
                        fn myeq(x: a, y: a): Bool; \
                        fn myneq(x: a, y: a): Bool; \
                    } \
                    impl MyEq for Foo { \
                        fn myeq(Bar, Bar) => true; \
                        fn myneq(x, y) => !myeq(x, y); \
                    }";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_default_impl_method() {
        let code = "trait Eq<a> { \
                        fn eq(x: a, y: a): Bool; \
                        fn neq(x: a, y: a): Bool => !(eq(x, y)); \
                    } \
                    impl Eq for Bool { fn eq(true, true) => true; fn eq(false, false) => true; fn eq(_, _) => false; } \
                    let r = neq(true, false);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_forward_reference() {
        let code = "fn a(n: Int): Int => b(n); fn b(n: Int): Int => n + 1;";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_mutual_recursion() {
        let code = "fn isEven(0): Bool => true; \
                    fn isEven(n: Int): Bool => isOdd(n - 1); \
                    fn isOdd(0): Bool => false; \
                    fn isOdd(n: Int): Bool => isEven(n - 1); \
                    let r = isEven(10);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_polymorphic_fn_used_at_two_types() {
        let code = "fn wrap(x: a): [a] => [x]; \
                    let xs = wrap(1); \
                    let ys = wrap(\"hi\");";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_forward_referenced_helper_generalizes() {
        let code = "fn myLen(xs: [a]): Int => myFold(fn (n, _) => n + 1, 0, xs); \
                    fn myFold(f: b -> a -> b, acc: b, []): b => acc; \
                    fn myFold(f: b -> a -> b, acc: b, [x | xs]): b => myFold(f, f(acc, x), xs); \
                    let p = myLen([1, 2, 3]); \
                    let q = myLen([[1], [2]]);";
        assert!(check_program(code).is_ok());
    }

    #[test]
    fn test_module_forward_reference() {
        let code = "module M; \
                    pub fn replicate(n: Int, x: a): [a] => go(n, x, []); \
                    fn go(0, _: a, acc: [a]): [a] => acc; \
                    fn go(n: Int, x: a, acc: [a]): [a] => go(n - 1, x, [x | acc]);";
        assert!(check_module(code).is_ok());
    }
}
