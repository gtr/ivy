pub const OR_OPERATOR: &str = "ivy spells logical or as `or`: `a or b`";

pub const LIST_HEADS: &str = "`|` separates one head from the rest. for more heads, nest them: `[a | [b | rest]]`";

pub const INTERPOLATED_PATTERN: &str =
    "an interpolated string can't be a pattern. bind a name instead and compare it in a guard";

pub const NO_RETURN: &str = "ivy has no `return`; a function's value is its body";

pub fn unexpected_char(ch: char) -> Option<&'static str> {
    match ch {
        '&' => Some("ivy spells logical and as `and`: `a and b`"),
        _ => None,
    }
}

pub fn keyword_as_name(keyword: &str) -> String {
    format!("`{keyword}` is a keyword, so it can't be used as a name")
}

pub fn foreign_name(name: &str) -> Option<&'static str> {
    match name {
        "null" | "nil" => {
            Some("ivy has no null. for a value that might be missing, use an Option: `Some(x)` or `None`")
        }
        "len" => Some("the length of a list is `length(xs)`"),
        "toString" | "to_string" => Some("turn a value into a string with `show(x)`, or put it in a string as \"{x}\""),
        "puts" | "printf" => Some("print a line with `println(x)`"),
        _ => None,
    }
}

pub fn import_module(name: &str, module: &str) -> String {
    format!("`{name}` is in the {module} module: add `import {module};` and call `{module}.{name}`")
}

pub fn module_member(module: &str, member: &str, suggestion: Option<&str>) -> String {
    match suggestion {
        Some(name) => format!("did you mean `{module}.{name}`?"),
        None => format!("check the spelling, or whether `{member}` is marked `pub` in {module}"),
    }
}

pub fn missing_prelude(path: &str) -> String {
    format!("warning: no prelude at {path}; run ivy from the repository root, or pass --no-prelude")
}
