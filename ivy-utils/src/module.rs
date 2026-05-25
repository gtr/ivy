use std::path::PathBuf;

/// Resolve a module path (e.g., `["Math"]` or `["Foo", "Bar"]`) to a file path
/// by searching the given directories. Tries lowercase first, then original case.
pub fn resolve_module_path(module_path: &[String], search_paths: &[PathBuf]) -> Option<PathBuf> {
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

    for base in search_paths {
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
