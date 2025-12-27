//! Module loader for Ivy.
//!
//! Handles loading, caching, and resolving module imports.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;

use ivy_syntax::{Decl, Program};

use crate::value::Value;

/// Error types for module loading.
#[derive(Debug, Clone)]
pub enum LoadError {
    /// File not found.
    NotFound {
        module_name: String,
        searched_paths: Vec<PathBuf>,
    },
    /// IO error reading the file.
    IoError { path: PathBuf, message: String },
    /// Parse error in the module.
    ParseError { path: PathBuf, message: String },
    /// Circular import detected.
    CircularImport { chain: Vec<String> },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::NotFound {
                module_name,
                searched_paths,
            } => {
                write!(f, "Module '{}' not found. Searched:", module_name)?;
                for path in searched_paths {
                    write!(f, "\n  - {}", path.display())?;
                }
                Ok(())
            }
            LoadError::IoError { path, message } => {
                write!(f, "Error reading '{}': {}", path.display(), message)
            }
            LoadError::ParseError { path, message } => {
                write!(f, "Parse error in '{}': {}", path.display(), message)
            }
            LoadError::CircularImport { chain } => {
                write!(f, "Circular import detected: {}", chain.join(" -> "))
            }
        }
    }
}

impl std::error::Error for LoadError {}

/// A loaded module with its exports.
#[derive(Debug, Clone)]
pub struct Module {
    /// The module name.
    pub name: String,
    /// The file path from which this module was loaded.
    pub path: PathBuf,
    /// The parsed program.
    pub program: Program,
    /// Exported (public) bindings: name -> value.
    pub exports: HashMap<String, Value>,
    /// All public declaration names.
    pub public_names: HashSet<String>,
}

impl Module {
    /// Create a new empty module.
    pub fn new(name: String, path: PathBuf, program: Program) -> Self {
        Module {
            name,
            path,
            program,
            exports: HashMap::new(),
            public_names: HashSet::new(),
        }
    }

    /// Collect public names from declarations.
    pub fn collect_public_names(&mut self) {
        for decl in &self.program.declarations {
            match &decl.node {
                Decl::Fn(fn_decl) if fn_decl.is_pub => {
                    self.public_names.insert(fn_decl.name.name.clone());
                }
                Decl::Let {
                    is_pub: true, pattern, ..
                } => {
                    collect_pattern_names(&pattern.node, &mut self.public_names);
                }
                Decl::Type {
                    is_pub: true,
                    name,
                    body,
                    ..
                } => {
                    self.public_names.insert(name.name.clone());
                    if let ivy_syntax::TypeBody::Sum(variants) = body {
                        for variant in variants {
                            self.public_names.insert(variant.name.name.clone());
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

/// Collect variable names from a pattern.
fn collect_pattern_names(pattern: &ivy_syntax::Pattern, names: &mut HashSet<String>) {
    match pattern {
        ivy_syntax::Pattern::Var(ident) => {
            names.insert(ident.name.clone());
        }
        ivy_syntax::Pattern::Tuple { elements } => {
            for pat in elements {
                collect_pattern_names(&pat.node, names);
            }
        }
        ivy_syntax::Pattern::List { elements } => {
            for pat in elements {
                collect_pattern_names(&pat.node, names);
            }
        }
        ivy_syntax::Pattern::Cons { head, tail } => {
            collect_pattern_names(&head.node, names);
            collect_pattern_names(&tail.node, names);
        }
        ivy_syntax::Pattern::Record { fields, .. } => {
            for field in fields {
                if let Some(pat) = &field.pattern {
                    collect_pattern_names(&pat.node, names);
                } else {
                    names.insert(field.name.name.clone());
                }
            }
        }
        _ => {}
    }
}

/// Module loader with caching and circular import detection.
pub struct ModuleLoader {
    /// Base paths for resolving imports (current directory, lib directory).
    search_paths: Vec<PathBuf>,
    /// Cache of loaded modules.
    loaded: HashMap<String, Module>,
    /// Stack of modules currently being loaded (for circular import detection).
    loading_stack: Vec<String>,
}

impl ModuleLoader {
    /// Create a new module loader with the given search paths.
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        ModuleLoader {
            search_paths,
            loaded: HashMap::new(),
            loading_stack: Vec::new(),
        }
    }

    /// Add a search path.
    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    /// Resolve a module path to a file path.
    pub fn resolve_path(&self, module_path: &[String]) -> Option<PathBuf> {
        if module_path.is_empty() {
            return None;
        }

        let lowercase_path: PathBuf = module_path
            .iter()
            .map(|s| s.to_lowercase())
            .collect::<Vec<_>>()
            .join("/")
            .into();

        let original_path: PathBuf = module_path.join("/").into();

        // TODO(gtr): decide case-sensitivity
        for base in &self.search_paths {
            let path1 = base.join(&lowercase_path).with_extension("ivy");
            if path1.exists() {
                return Some(path1);
            }
            let path2 = base.join(&original_path).with_extension("ivy");
            if path2.exists() {
                return Some(path2);
            }
        }

        None
    }

    /// Get all searched paths for error messages.
    fn get_searched_paths(&self, module_path: &[String]) -> Vec<PathBuf> {
        let lowercase_path: PathBuf = module_path
            .iter()
            .map(|s| s.to_lowercase())
            .collect::<Vec<_>>()
            .join("/")
            .into();

        let original_path: PathBuf = module_path.join("/").into();

        let mut paths = Vec::new();
        for base in &self.search_paths {
            paths.push(base.join(&lowercase_path).with_extension("ivy"));
            paths.push(base.join(&original_path).with_extension("ivy"));
        }
        paths
    }

    /// Load a module by its path segments.
    pub fn load(&mut self, module_path: &[String]) -> Result<&Module, LoadError> {
        let module_name = module_path.join(".");

        if self.loaded.contains_key(&module_name) {
            return Ok(self.loaded.get(&module_name).unwrap());
        }

        if self.loading_stack.contains(&module_name) {
            let mut chain = self.loading_stack.clone();
            chain.push(module_name.clone());
            return Err(LoadError::CircularImport { chain });
        }

        let file_path = self.resolve_path(module_path).ok_or_else(|| LoadError::NotFound {
            module_name: module_name.clone(),
            searched_paths: self.get_searched_paths(module_path),
        })?;

        let source = fs::read_to_string(&file_path).map_err(|e| LoadError::IoError {
            path: file_path.clone(),
            message: e.to_string(),
        })?;

        let program = ivy_parse::parse(&source).map_err(|e| LoadError::ParseError {
            path: file_path.clone(),
            message: format!("{:?}", e),
        })?;

        let mut module = Module::new(module_name.clone(), file_path, program);
        module.collect_public_names();

        self.loading_stack.push(module_name.clone());
        self.loaded.insert(module_name.clone(), module);
        self.loading_stack.pop();

        Ok(self.loaded.get(&module_name).unwrap())
    }

    /// Check if a module is loaded.
    pub fn is_loaded(&self, module_name: &str) -> bool {
        self.loaded.contains_key(module_name)
    }

    /// Get a loaded module.
    pub fn get(&self, module_name: &str) -> Option<&Module> {
        self.loaded.get(module_name)
    }

    /// Get a mutable reference to a loaded module.
    pub fn get_mut(&mut self, module_name: &str) -> Option<&mut Module> {
        self.loaded.get_mut(module_name)
    }
}
