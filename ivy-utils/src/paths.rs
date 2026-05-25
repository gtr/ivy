use std::env;
use std::path::PathBuf;

/// Get the default search paths for module resolution.
///
/// Returns paths in priority order:
/// 1. Current working directory
/// 2. `lib/` under the current working directory
/// 3. `lib/` next to the executable
/// 4. `lib/` one level above the executable (for development layouts)
pub fn get_default_search_paths() -> Vec<PathBuf> {
    let mut paths = vec![];
    if let Ok(cwd) = env::current_dir() {
        paths.push(cwd.clone());
        paths.push(cwd.join("lib"));
    }

    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            paths.push(exe_dir.join("lib"));
            if let Some(parent) = exe_dir.parent() {
                paths.push(parent.join("lib"));
            }
        }
    }

    paths
}
