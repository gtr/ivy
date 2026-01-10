//! Type inference using Algorithm W (Hindley-Milner).
//!
//! This module implements constraint-based type inference:
//! 1. Walk the AST, generating type variables and constraints
//! 2. Solve constraints via unification
//! 3. Apply the resulting substitution to get final types
//! https://smunix.github.io/dev.stephendiehl.com/fun/006_hindley_milner.html

use ivy_syntax::{
    expr::{Expr, MatchArm, Param},
    lit::Literal,
    op::{BinOp, UnaryOp},
    pattern::Pattern,
    Span, Spanned,
};

use crate::env::{TypeEnv, TypeVarGen};
use crate::error::{TypeError, TypeResult};
use crate::exhaustiveness;
use crate::registry::TypeRegistry;
use crate::subst::Subst;
use crate::types::{Scheme, Type};
use crate::unify::unify_with_subst;

/// Type checker state.
pub struct TypeChecker {
    /// Type variable generator.
    gen: TypeVarGen,
    /// Current substitution (accumulates constraints).
    pub subst: Subst,
    /// Type registry for exhaustiveness checking.
    pub registry: TypeRegistry,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> TypeChecker {
        TypeChecker {
            gen: TypeVarGen::new(),
            subst: Subst::new(),
            registry: TypeRegistry::with_builtins(),
        }
    }

    /// Infer the type of an expression.
    pub fn infer(&mut self, expr: &Spanned<Expr>, env: &TypeEnv) -> TypeResult<Type> {
        let span = expr.span;
        match &expr.node {
            Expr::Lit(lit) => Ok(self.infer_lit(lit)),

            Expr::Var(ident) => {
                let name = &ident.name;
                match env.get(name) {
                    Some(scheme) => Ok(self.gen.instantiate(scheme)),
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

            Expr::Match { scrutinee, arms } => self.infer_match(scrutinee, arms, env, span),

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
                // TODO(gtr): For now, records are structural; we'll need type definitions later
                let mut field_types = Vec::new();
                for field in fields {
                    let ty = self.infer(&field.value, env)?;
                    field_types.push((field.name.name.clone(), ty));
                }
                Ok(Type::Record(name.name.clone(), field_types))
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

                let resolved = self.subst.apply(&left_ty);
                match resolved {
                    Type::Int | Type::Float => Ok(resolved),
                    Type::Var(_) => {
                        unify_with_subst(&resolved, &Type::Int, &mut self.subst, span)?;
                        Ok(Type::Int)
                    }
                    _ => Err(TypeError::mismatch(Type::Int, resolved, span)),
                }
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
                match &resolved {
                    Type::List(_) | Type::String => Ok(resolved),
                    Type::Var(_) => {
                        let elem = self.gen.fresh_type();
                        let list_ty = Type::list(elem);
                        unify_with_subst(&resolved, &list_ty, &mut self.subst, span)?;
                        Ok(list_ty)
                    }
                    _ => Err(TypeError::mismatch(Type::String, resolved, span)),
                }
            }
        }
    }

    /// Infer the type of a unary expression.
    fn infer_unary(&mut self, op: UnaryOp, operand: &Spanned<Expr>, env: &TypeEnv, span: Span) -> TypeResult<Type> {
        let operand_ty = self.infer(operand, env)?;

        match op {
            // Negation: Int -> Int or Float -> Float
            UnaryOp::Neg => {
                let resolved = self.subst.apply(&operand_ty);
                match resolved {
                    Type::Int | Type::Float => Ok(resolved),
                    Type::Var(_) => {
                        unify_with_subst(&resolved, &Type::Int, &mut self.subst, span)?;
                        Ok(Type::Int)
                    }
                    _ => Err(TypeError::mismatch(Type::Int, resolved, span)),
                }
            }
            // Not: Bool -> Bool
            UnaryOp::Not => {
                unify_with_subst(&operand_ty, &Type::Bool, &mut self.subst, operand.span)?;
                Ok(Type::Bool)
            }
        }
    }

    /// Infer the type of a let binding.
    fn infer_let(
        &mut self,
        pattern: &Spanned<Pattern>,
        ty_ann: Option<&Spanned<ivy_syntax::types::TypeExpr>>,
        value: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> TypeResult<Type> {
        let value_ty = self.infer(value, env)?;

        if let Some(ann) = ty_ann {
            let ann_ty = self.type_expr_to_type(&ann.node, env);
            unify_with_subst(&value_ty, &ann_ty, &mut self.subst, ann.span)?;
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

    /// Infer the type of an assignment.
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

    /// Infer the type of an if expression.
    fn infer_if(
        &mut self,
        condition: &Spanned<Expr>,
        then_branch: &Spanned<Expr>,
        else_branch: &Spanned<Expr>,
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
        let cond_ty = self.infer(condition, env)?;
        unify_with_subst(&cond_ty, &Type::Bool, &mut self.subst, condition.span)?;

        let then_ty = self.infer(then_branch, env)?;
        let else_ty = self.infer(else_branch, env)?;
        unify_with_subst(&then_ty, &else_ty, &mut self.subst, span)?;

        Ok(then_ty)
    }

    /// Infer the type of a match expression.
    fn infer_match(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[MatchArm],
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
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

        // Check exhaustiveness
        let patterns: Vec<&Pattern> = arms.iter().map(|arm| &arm.pattern.node).collect();
        let resolved_ty = self.subst.apply(&scrutinee_ty);
        exhaustiveness::check_exhaustiveness(&resolved_ty, &patterns, &self.registry, span)?;

        Ok(result_ty.unwrap_or_else(|| self.gen.fresh_type()))
    }

    /// Infer the type of a lambda expression.
    fn infer_lambda(
        &mut self,
        params: &[Param],
        return_ty: Option<&Spanned<ivy_syntax::types::TypeExpr>>,
        body: &Spanned<Expr>,
        env: &TypeEnv,
    ) -> TypeResult<Type> {
        // Generate types for parameters
        let mut param_types = Vec::new();
        let mut bindings = Vec::new();

        for param in params {
            let ty = if let Some(ann) = &param.ty {
                self.type_expr_to_type(&ann.node, env)
            } else {
                self.gen.fresh_type()
            };

            // Extract variable name from pattern
            if let Pattern::Var(ident) = &param.pattern.node {
                bindings.push((ident.name.clone(), Scheme::mono(ty.clone())));
            }

            param_types.push(ty);
        }

        let body_env = env.extend(bindings);
        let body_ty = self.infer(body, &body_env)?;

        if let Some(ann) = return_ty {
            let ann_ty = self.type_expr_to_type(&ann.node, env);
            unify_with_subst(&body_ty, &ann_ty, &mut self.subst, ann.span)?;
        }

        let mut result = body_ty;
        for param_ty in param_types.into_iter().rev() {
            result = Type::fun(param_ty, result);
        }

        Ok(result)
    }

    /// Infer the type of a function call.
    fn infer_call(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<Expr>],
        env: &TypeEnv,
        span: Span,
    ) -> TypeResult<Type> {
        let callee_ty = self.infer(callee, env)?;

        // Infer argument types
        let mut arg_types = Vec::new();
        for arg in args {
            arg_types.push(self.infer(arg, env)?);
        }

        // Build expected function type: arg1 -> arg2 -> ... -> result
        let result_ty = self.gen.fresh_type();
        let mut expected = result_ty.clone();
        for arg_ty in arg_types.into_iter().rev() {
            expected = Type::fun(arg_ty, expected);
        }

        unify_with_subst(&callee_ty, &expected, &mut self.subst, span)?;

        Ok(result_ty)
    }

    /// Infer the type of a field access.
    fn infer_field(&mut self, object: &Spanned<Expr>, field: &str, env: &TypeEnv, span: Span) -> TypeResult<Type> {
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
            last_ty = self.infer(expr, &current_env)?;

            // If it's a let binding, extend the environment
            if let Expr::Let { pattern, value, ty, .. } = &expr.node {
                let value_ty = self.infer(value, &current_env)?;
                if let Some(ann) = ty {
                    let ann_ty = self.type_expr_to_type(&ann.node, &current_env);
                    unify_with_subst(&value_ty, &ann_ty, &mut self.subst, ann.span)?;
                }

                // Collect bindings from pattern (handles tuples, etc.)
                let bindings = self.infer_pattern(pattern, &value_ty, &current_env)?;
                for (name, scheme) in bindings {
                    current_env.insert(name, scheme);
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
    fn infer_list(&mut self, elements: &[Spanned<Expr>], env: &TypeEnv, span: Span) -> TypeResult<Type> {
        if elements.is_empty() {
            return Ok(Type::list(self.gen.fresh_type()));
        }

        let first_ty = self.infer(&elements[0], env)?;

        for elem in &elements[1..] {
            let elem_ty = self.infer(elem, env)?;
            unify_with_subst(&first_ty, &elem_ty, &mut self.subst, span)?;
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
                    let ctor_ty = self.instantiate(scheme);

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
                                    arg_types.push(self.subst.apply(&*arg_ty));
                                    current_ty = self.subst.apply(&*ret_ty);
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
                for field in fields {
                    let field_ty = self.gen.fresh_type();
                    if let Some(pat) = &field.pattern {
                        self.collect_pattern_bindings(pat, &field_ty, bindings, env)?;
                    } else {
                        bindings.push((field.name.name.clone(), Scheme::mono(field_ty)));
                    }
                }
                let _ = name;
                Ok(())
            }

            Pattern::Or { left, right } => {
                // Both sides must produce same bindings with same types
                let mut left_bindings = Vec::new();
                let mut right_bindings = Vec::new();
                self.collect_pattern_bindings(left, &expected, &mut left_bindings, env)?;
                self.collect_pattern_bindings(right, &expected, &mut right_bindings, env)?;
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

    /// Generate a fresh type variable.
    pub fn fresh_type(&mut self) -> Type {
        self.gen.fresh_type()
    }

    /// Generate a fresh type variable ID.
    pub fn fresh_var(&mut self) -> crate::types::TypeVar {
        self.gen.fresh()
    }

    /// Instantiate a type scheme with fresh type variables.
    pub fn instantiate(&mut self, scheme: &Scheme) -> Type {
        self.gen.instantiate(scheme)
    }

    /// Convert a type expression to a Type.
    pub fn type_expr_to_type(&mut self, ty_expr: &ivy_syntax::types::TypeExpr, env: &TypeEnv) -> Type {
        self.type_expr_to_type_scoped(ty_expr, env, None)
    }

    /// Convert a type expression to a Type with optional type variable scoping.
    pub fn type_expr_to_type_scoped(
        &mut self,
        ty_expr: &ivy_syntax::types::TypeExpr,
        env: &TypeEnv,
        mut scope: Option<&mut std::collections::HashMap<String, Type>>,
    ) -> Type {
        use ivy_syntax::types::TypeExpr;

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
                        if let Some(scheme) = env.get(name) {
                            self.instantiate(scheme)
                        } else if let Some(ref mut sc) = scope {
                            // If we have a scope and name looks like a type variable,
                            // use consistent type variables
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
                    // type_args.push(self.type_expr_to_type_scoped(&a.node, env, scope.as_deref_mut()));
                    type_args.push(self.type_expr_to_type_scoped(&a.node, env, scope.as_deref_mut()));
                }
                Type::Named(name.clone(), type_args)
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

    /// Check if a name looks like a type variable (short uppercase name, 1-3 chars).
    fn looks_like_type_var(name: &str) -> bool {
        name.len() <= 3 && name.chars().all(|c| c.is_uppercase())
    }

    /// Get the final type after applying all substitutions.
    pub fn finalize(&self, ty: &Type) -> Type {
        self.subst.apply(ty)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        TypeChecker::new()
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
    fn test_infer_int() {
        assert_eq!(check("42;").unwrap(), Type::Int);
    }

    #[test]
    fn test_infer_bool() {
        assert_eq!(check("true;").unwrap(), Type::Bool);
    }

    #[test]
    fn test_infer_string() {
        assert_eq!(check("\"hello\";").unwrap(), Type::String);
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
}
