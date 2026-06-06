//! Shared module loader with caching.

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::fs;
use std::path::PathBuf;

use ivy_syntax::{collect_public_names, Program};

use ivy_utils::resolve_module_path;

/// A parsed module cached by the loader.
#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub program: Program,
    pub public_names: HashSet<String>,
}

/// Errors from module loading.
#[derive(Debug, Clone)]
pub enum ModuleLoadError {
    NotFound {
        module_name: String,
        searched_paths: Vec<PathBuf>,
    },
    IoError {
        path: PathBuf,
        message: String,
    },
    ParseError {
        path: PathBuf,
        message: String,
    },
}

impl fmt::Display for ModuleLoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModuleLoadError::NotFound {
                module_name,
                searched_paths,
            } => {
                write!(f, "Module '{}' not found. Searched:", module_name)?;
                for path in searched_paths {
                    write!(f, "\n  - {}", path.display())?;
                }
                Ok(())
            }
            ModuleLoadError::IoError { path, message } => {
                write!(f, "Error reading '{}': {}", path.display(), message)
            }
            ModuleLoadError::ParseError { path, message } => {
                write!(f, "Parse error in '{}': {}", path.display(), message)
            }
        }
    }
}

impl Error for ModuleLoadError {}

/// Module loader with caching. Shared between the type checker and evaluator.
pub struct ModuleLoader {
    search_paths: Vec<PathBuf>,
    loaded: HashMap<String, ParsedModule>,
}

impl ModuleLoader {
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        ModuleLoader {
            search_paths,
            loaded: HashMap::new(),
        }
    }

    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    pub fn search_paths(&self) -> &[PathBuf] {
        &self.search_paths
    }

    /// Load and parse a module, returning a cached reference.
    pub fn load(&mut self, module_path: &[String]) -> Result<&ParsedModule, ModuleLoadError> {
        let module_name = module_path.join(".");

        if self.loaded.contains_key(&module_name) {
            return Ok(self.loaded.get(&module_name).unwrap());
        }

        let file_path =
            resolve_module_path(module_path, &self.search_paths).ok_or_else(|| ModuleLoadError::NotFound {
                module_name: module_name.clone(),
                searched_paths: self.get_searched_paths(module_path),
            })?;

        let source = fs::read_to_string(&file_path).map_err(|e| ModuleLoadError::IoError {
            path: file_path.clone(),
            message: e.to_string(),
        })?;

        let program = crate::parse(&source).map_err(|e| ModuleLoadError::ParseError {
            path: file_path.clone(),
            message: format!("{:?}", e),
        })?;

        let public_names = collect_public_names(&program.declarations);

        let module = ParsedModule {
            name: module_name.clone(),
            path: file_path,
            source,
            program,
            public_names,
        };

        self.loaded.insert(module_name.clone(), module);
        Ok(self.loaded.get(&module_name).unwrap())
    }

    pub fn get(&self, module_name: &str) -> Option<&ParsedModule> {
        self.loaded.get(module_name)
    }

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
}
