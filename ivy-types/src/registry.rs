//! Type registry for tracking type definitions.

use std::collections::HashMap;

/// Information about a constructor.
#[derive(Debug, Clone)]
pub struct ConstructorInfo {
    /// The type this constructor belongs to.
    pub type_name: String,
    /// Number of fields/arguments.
    pub arity: usize,
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

    /// Register a sum type with its variants.
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

    /// Register a sum type from parsed variants.
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
        self.constructors.get(type_name).map(|v| v.as_slice())
    }

    pub fn get_constructor_info(&self, ctor_name: &str) -> Option<&ConstructorInfo> {
        self.constructor_info.get(ctor_name)
    }

    pub fn is_sum_type(&self, type_name: &str) -> bool {
        self.constructors.contains_key(type_name)
    }

    pub fn constructor_count(&self, type_name: &str) -> usize {
        self.constructors.get(type_name).map(|v| v.len()).unwrap_or(0)
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
