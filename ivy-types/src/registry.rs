//! Type registry for tracking type definitions.

use ivy_syntax::decl::FnDecl;
use ivy_syntax::Span;
use std::collections::HashMap;

/// Information about a constructor.
#[derive(Debug, Clone)]
pub struct ConstructorInfo {
    /// The type this constructor belongs to.
    pub type_name: String,
    /// Number of fields/arguments.
    pub arity: usize,
}

/// Information about a record field.
#[derive(Debug, Clone)]
pub struct RecordFieldInfo {
    /// The field name
    pub name: String,
    /// The field type. May reference type parameters from the enclosing record
    pub ty: crate::Type,
}

/// Information about a record type definition
#[derive(Debug, Clone)]
pub struct RecordInfo {
    /// The record's type parameters: `[a, b]` for `type Pair<a, b>`
    pub params: Vec<crate::TypeVar>,
    /// The record's fields, in declaration order
    pub fields: Vec<RecordFieldInfo>,
}

/// Information about a type alias
#[derive(Debug, Clone)]
pub struct AliasInfo {
    /// Type parameters bound by the alias (their TypeVar identities)
    pub params: Vec<crate::TypeVar>,
    /// The aliased type, with `params` referenced via `Type::Var`
    pub body: crate::Type,
}

/// Information about a trait decl
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub name: String,
    pub param: crate::TypeVar,
    pub methods: HashMap<String, crate::Scheme>,
    pub default_impls: HashMap<String, FnDecl>,
    pub span: Span,
}

/// Information about a single impl
#[derive(Debug, Clone)]
pub struct ImplInfo {
    pub trait_name: String,
    /// Canonicalized impl head
    pub head: crate::Type,
    /// Free vars introduced by this impl (the `a` in `impl Show for Option<a>`)
    pub head_vars: Vec<crate::TypeVar>,
    /// Where-clause constraints with type args using `head_vars`
    pub where_constraints: Vec<crate::TraitConstraint>,
    pub span: Span,
}

/// Registry of type definitions.
///
/// Maintains mappings from type names -> constructors and vice versa
/// Used for exhaustiveness checking in pattern matching.
#[derive(Debug, Clone, Default)]
pub struct TypeRegistry {
    /// Maps type name -> list of constructor names (in declaration order)
    constructors: HashMap<String, Vec<String>>,

    /// Maps constructor name -> constructor info
    constructor_info: HashMap<String, ConstructorInfo>,

    /// Maps record type name -> info (params + fields, in declaration order)
    records: HashMap<String, RecordInfo>,

    /// Maps type alias name -> alias info
    aliases: HashMap<String, AliasInfo>,

    /// Maps trait name -> trait info
    traits: HashMap<String, TraitInfo>,

    /// Maps trait name -> list of impls (multiple impls per trait, one per type)
    impls: HashMap<String, Vec<ImplInfo>>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a registry with built-in types.
    pub fn with_builtins() -> Self {
        let mut registry = Self::new();

        registry.register_sum_type("Option", &[("None", 0), ("Some", 1)]);
        registry.register_sum_type("Result", &[("Ok", 1), ("Err", 1)]);
        registry.register_sum_type("Ordering", &[("Less", 0), ("Equal", 0), ("Greater", 0)]);
        registry
    }

    /// Register a sum type with its variants
    pub fn register_sum_type(&mut self, type_name: &str, variants: &[(&str, usize)]) {
        let mut ctor_names = Vec::with_capacity(variants.len());

        for (ctor_name, arity) in variants {
            ctor_names.push(ctor_name.to_string());
            self.constructor_info.insert(
                ctor_name.to_string(),
                ConstructorInfo {
                    type_name: type_name.to_string(),
                    arity: *arity,
                },
            );
        }

        self.constructors.insert(type_name.to_string(), ctor_names);
    }

    /// Register a sum type from parsed variants
    pub fn register_from_variants(&mut self, type_name: &str, variants: &[crate::VariantInfo]) {
        let mut ctor_names = Vec::with_capacity(variants.len());

        for variant in variants {
            ctor_names.push(variant.name.clone());
            self.constructor_info.insert(
                variant.name.clone(),
                ConstructorInfo {
                    type_name: type_name.to_string(),
                    arity: variant.arity,
                },
            );
        }

        self.constructors.insert(type_name.to_string(), ctor_names);
    }

    pub fn get_constructors(&self, type_name: &str) -> Option<&[String]> {
        self.constructors.get(type_name).map(Vec::as_slice)
    }

    pub fn get_constructor_info(&self, ctor_name: &str) -> Option<&ConstructorInfo> {
        self.constructor_info.get(ctor_name)
    }

    pub fn is_sum_type(&self, type_name: &str) -> bool {
        self.constructors.contains_key(type_name)
    }

    pub fn constructor_count(&self, type_name: &str) -> usize {
        self.constructors.get(type_name).map(Vec::len).unwrap_or(0)
    }

    /// Register a record type with its parameters and fields
    pub fn register_record(&mut self, type_name: &str, params: Vec<crate::TypeVar>, fields: &[(String, crate::Type)]) {
        let field_infos: Vec<RecordFieldInfo> = fields
            .iter()
            .map(|(name, ty)| RecordFieldInfo {
                name: name.clone(),
                ty: ty.clone(),
            })
            .collect();
        self.records.insert(
            type_name.to_string(),
            RecordInfo {
                params,
                fields: field_infos,
            },
        );
    }

    pub fn get_record(&self, type_name: &str) -> Option<&RecordInfo> {
        self.records.get(type_name)
    }

    pub fn get_record_fields(&self, type_name: &str) -> Option<&[RecordFieldInfo]> {
        self.records.get(type_name).map(|r| r.fields.as_slice())
    }

    pub fn is_record_type(&self, type_name: &str) -> bool {
        self.records.contains_key(type_name)
    }

    pub fn register_alias(&mut self, type_name: &str, params: Vec<crate::TypeVar>, body: crate::Type) {
        self.aliases.insert(type_name.to_string(), AliasInfo { params, body });
    }

    pub fn get_alias(&self, type_name: &str) -> Option<&AliasInfo> {
        self.aliases.get(type_name)
    }

    pub fn register_trait(&mut self, info: TraitInfo) {
        self.traits.insert(info.name.clone(), info);
    }

    pub fn get_trait(&self, name: &str) -> Option<&TraitInfo> {
        self.traits.get(name)
    }

    pub fn is_trait(&self, name: &str) -> bool {
        self.traits.contains_key(name)
    }

    pub fn register_impl(&mut self, info: ImplInfo) {
        self.impls.entry(info.trait_name.clone()).or_default().push(info);
    }

    pub fn get_impls(&self, trait_name: &str) -> &[ImplInfo] {
        match self.impls.get(trait_name) {
            Some(v) => v.as_slice(),
            None => &[],
        }
    }

    pub fn all_impls(&self) -> impl Iterator<Item = (&str, &ImplInfo)> {
        self.impls
            .iter()
            .flat_map(|(name, impls)| impls.iter().map(move |i| (name.as_str(), i)))
    }
}

/// Simplified variant info for registration.
#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub name: String,
    pub arity: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_sum_type() {
        let mut registry = TypeRegistry::new();
        registry.register_sum_type("Color", &[("Red", 0), ("Green", 0), ("Blue", 0)]);

        let ctors = registry.get_constructors("Color").unwrap();
        assert_eq!(ctors, &["Red", "Green", "Blue"]);

        let info = registry.get_constructor_info("Red").unwrap();
        assert_eq!(info.type_name, "Color");
        assert_eq!(info.arity, 0);
    }

    #[test]
    fn test_builtins() {
        let registry = TypeRegistry::with_builtins();

        let option_ctors = registry.get_constructors("Option").unwrap();
        assert_eq!(option_ctors, &["None", "Some"]);

        let some_info = registry.get_constructor_info("Some").unwrap();
        assert_eq!(some_info.type_name, "Option");
        assert_eq!(some_info.arity, 1);
    }
}
