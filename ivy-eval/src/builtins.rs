//! Built-in functions for Ivy.
//! TODO(gtr): wayyyy too much of this is being done in Rust. Eventually, we want to write as
//! much of the standard library + built-in functions in ivy. For now, we'll
//! label this as tech-debt and move on. A proper refactor *will* be done.

use std::io::{self, Write};

use crate::error::{EvalError, EvalResult};
use crate::value::{vec_to_list, BuiltinFn, Value};
use ivy_syntax::Span;
use rand::Rng;

/// Print without newline.
pub static BUILTIN_PRINT: BuiltinFn = BuiltinFn {
    name: "__print",
    arity: 1,
    func: builtin_print,
};

/// Print with newline.
pub static BUILTIN_PRINTLN: BuiltinFn = BuiltinFn {
    name: "__println",
    arity: 1,
    func: builtin_println,
};

/// Convert int to string.
pub static BUILTIN_INT_TO_STRING: BuiltinFn = BuiltinFn {
    name: "__intToString",
    arity: 1,
    func: builtin_int_to_string,
};

/// Read a line from stdin.
pub static BUILTIN_READ_LINE: BuiltinFn = BuiltinFn {
    name: "__readLine",
    arity: 0,
    func: builtin_read_line,
};

/// Prompt and read an int.
pub static BUILTIN_READ_INT: BuiltinFn = BuiltinFn {
    name: "__readInt",
    arity: 1,
    func: builtin_read_int,
};

/// Convert any value to a string.
pub static BUILTIN_SHOW: BuiltinFn = BuiltinFn {
    name: "show",
    arity: 1,
    func: builtin_show,
};

fn builtin_print(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(s) => print!("{}", s),
        v => print!("{}", v),
    }
    io::stdout().flush().ok();
    Ok(Value::Unit)
}

fn builtin_println(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(s) => println!("{}", s),
        v => println!("{}", v),
    }
    Ok(Value::Unit)
}

fn builtin_int_to_string(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::Int(n) => Ok(Value::String(n.to_string())),
        v => Err(EvalError::TypeError {
            expected: "Int".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_read_line(_args: &[Value]) -> EvalResult<Value> {
    let mut input = String::new();
    io::stdin().read_line(&mut input).ok();
    Ok(Value::String(input.trim_end().to_string()))
}

fn builtin_read_int(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(prompt) => {
            print!("{}", prompt);
            io::stdout().flush().ok();
            let mut input = String::new();
            io::stdin().read_line(&mut input).ok();
            match input.trim().parse::<i64>() {
                Ok(n) => Ok(Value::Int(n)),
                Err(_) => Ok(Value::Int(0)), // Could return error instead
            }
        }
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_show(args: &[Value]) -> EvalResult<Value> {
    Ok(Value::String(format!("{}", args[0])))
}

/// Absolute value
pub static BUILTIN_ABS: BuiltinFn = BuiltinFn {
    name: "abs",
    arity: 1,
    func: builtin_abs,
};

/// Minimum of two numbers
pub static BUILTIN_MIN: BuiltinFn = BuiltinFn {
    name: "min",
    arity: 2,
    func: builtin_min,
};

/// Maximum of two numbers
pub static BUILTIN_MAX: BuiltinFn = BuiltinFn {
    name: "max",
    arity: 2,
    func: builtin_max,
};

/// Power (base^exponent)
pub static BUILTIN_POW: BuiltinFn = BuiltinFn {
    name: "pow",
    arity: 2,
    func: builtin_pow,
};

/// Square root
pub static BUILTIN_SQRT: BuiltinFn = BuiltinFn {
    name: "sqrt",
    arity: 1,
    func: builtin_sqrt,
};

/// Floor (round down)
pub static BUILTIN_FLOOR: BuiltinFn = BuiltinFn {
    name: "floor",
    arity: 1,
    func: builtin_floor,
};

/// Ceiling (round up)
pub static BUILTIN_CEIL: BuiltinFn = BuiltinFn {
    name: "ceil",
    arity: 1,
    func: builtin_ceil,
};

/// Round to nearest integer
pub static BUILTIN_ROUND: BuiltinFn = BuiltinFn {
    name: "round",
    arity: 1,
    func: builtin_round,
};

/// Random integer in range [min, max]
pub static BUILTIN_RANDOM: BuiltinFn = BuiltinFn {
    name: "random",
    arity: 2,
    func: builtin_random,
};

fn builtin_abs(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        v => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_min(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).min(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.min(*b as f64))),
        _ => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_max(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).max(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.max(*b as f64))),
        _ => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_pow(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Int(base), Value::Int(exp)) if *exp >= 0 => Ok(Value::Int(base.pow(*exp as u32))),
        (Value::Int(base), Value::Int(exp)) => Ok(Value::Float((*base as f64).powf(*exp as f64))),
        (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
        (Value::Int(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
        (Value::Float(base), Value::Int(exp)) => Ok(Value::Float(base.powf(*exp as f64))),
        _ => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_sqrt(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
        Value::Float(f) => Ok(Value::Float(f.sqrt())),
        v => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_floor(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
        v => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_ceil(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
        v => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_round(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.round() as i64)),
        v => Err(EvalError::TypeError {
            expected: "Int or Float".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_random(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::Int(min), Value::Int(max)) => {
            let mut rng = rand::thread_rng();
            Ok(Value::Int(rng.gen_range(*min..=*max)))
        }
        _ => Err(EvalError::TypeError {
            expected: "Int, Int".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

/// String length
pub static BUILTIN_STR_LENGTH: BuiltinFn = BuiltinFn {
    name: "strLength",
    arity: 1,
    func: builtin_str_length,
};

/// String trim (whitespace)
pub static BUILTIN_STR_TRIM: BuiltinFn = BuiltinFn {
    name: "strTrim",
    arity: 1,
    func: builtin_str_trim,
};

/// String contains
pub static BUILTIN_STR_CONTAINS: BuiltinFn = BuiltinFn {
    name: "strContains",
    arity: 2,
    func: builtin_str_contains,
};

/// String substring
pub static BUILTIN_STR_SUBSTRING: BuiltinFn = BuiltinFn {
    name: "strSubstring",
    arity: 3,
    func: builtin_str_substring,
};

/// String split
pub static BUILTIN_STR_SPLIT: BuiltinFn = BuiltinFn {
    name: "strSplit",
    arity: 2,
    func: builtin_str_split,
};

/// String to uppercase
pub static BUILTIN_STR_TO_UPPER: BuiltinFn = BuiltinFn {
    name: "strToUpper",
    arity: 1,
    func: builtin_str_to_upper,
};

/// String to lowercase
pub static BUILTIN_STR_TO_LOWER: BuiltinFn = BuiltinFn {
    name: "strToLower",
    arity: 1,
    func: builtin_str_to_lower,
};

/// String starts with
pub static BUILTIN_STR_STARTS_WITH: BuiltinFn = BuiltinFn {
    name: "strStartsWith",
    arity: 2,
    func: builtin_str_starts_with,
};

/// String ends with
pub static BUILTIN_STR_ENDS_WITH: BuiltinFn = BuiltinFn {
    name: "strEndsWith",
    arity: 2,
    func: builtin_str_ends_with,
};

/// String replace
pub static BUILTIN_STR_REPLACE: BuiltinFn = BuiltinFn {
    name: "strReplace",
    arity: 3,
    func: builtin_str_replace,
};

fn builtin_str_length(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_str_trim(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.trim().to_string())),
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_str_contains(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::String(haystack), Value::String(needle)) => Ok(Value::Bool(haystack.contains(needle.as_str()))),
        _ => Err(EvalError::TypeError {
            expected: "String, String".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_str_substring(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Int(start), Value::Int(end)) => {
            let start = *start as usize;
            let end = *end as usize;
            if start > s.len() || end > s.len() || start > end {
                Ok(Value::String(String::new()))
            } else {
                Ok(Value::String(s[start..end].to_string()))
            }
        }
        _ => Err(EvalError::TypeError {
            expected: "String, Int, Int".to_string(),
            found: format!(
                "{}, {}, {}",
                args[0].type_name(),
                args[1].type_name(),
                args[2].type_name()
            ),
            span: Span::default(),
        }),
    }
}

fn builtin_str_split(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(delimiter)) => {
            let parts: Vec<Value> = s
                .split(delimiter.as_str())
                .map(|p| Value::String(p.to_string()))
                .collect();
            Ok(vec_to_list(parts))
        }
        _ => Err(EvalError::TypeError {
            expected: "String, String".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_str_to_upper(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_uppercase())),
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_str_to_lower(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_lowercase())),
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_str_starts_with(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(prefix)) => Ok(Value::Bool(s.starts_with(prefix.as_str()))),
        _ => Err(EvalError::TypeError {
            expected: "String, String".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_str_ends_with(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(suffix)) => Ok(Value::Bool(s.ends_with(suffix.as_str()))),
        _ => Err(EvalError::TypeError {
            expected: "String, String".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_str_replace(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::String(from), Value::String(to)) => {
            Ok(Value::String(s.replace(from.as_str(), to.as_str())))
        }
        _ => Err(EvalError::TypeError {
            expected: "String, String, String".to_string(),
            found: format!(
                "{}, {}, {}",
                args[0].type_name(),
                args[1].type_name(),
                args[2].type_name()
            ),
            span: Span::default(),
        }),
    }
}

/// Read entire file as string
pub static BUILTIN_READ_FILE: BuiltinFn = BuiltinFn {
    name: "readFile",
    arity: 1,
    func: builtin_read_file,
};

/// Write string to file
pub static BUILTIN_WRITE_FILE: BuiltinFn = BuiltinFn {
    name: "writeFile",
    arity: 2,
    func: builtin_write_file,
};

/// Append string to file
pub static BUILTIN_APPEND_FILE: BuiltinFn = BuiltinFn {
    name: "appendFile",
    arity: 2,
    func: builtin_append_file,
};

/// Check if file exists
pub static BUILTIN_FILE_EXISTS: BuiltinFn = BuiltinFn {
    name: "fileExists",
    arity: 1,
    func: builtin_file_exists,
};

fn builtin_read_file(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(path) => match std::fs::read_to_string(path) {
            Ok(content) => Ok(Value::String(content)),
            Err(e) => Err(EvalError::ModuleError {
                message: format!("Failed to read file '{}': {}", path, e),
                span: Span::default(),
            }),
        },
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}

fn builtin_write_file(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => match std::fs::write(path, content) {
            Ok(_) => Ok(Value::Unit),
            Err(e) => Err(EvalError::ModuleError {
                message: format!("Failed to write file '{}': {}", path, e),
                span: Span::default(),
            }),
        },
        _ => Err(EvalError::TypeError {
            expected: "String, String".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_append_file(args: &[Value]) -> EvalResult<Value> {
    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            use std::fs::OpenOptions;
            use std::io::Write as IoWrite;

            match OpenOptions::new().create(true).append(true).open(path) {
                Ok(mut file) => match file.write_all(content.as_bytes()) {
                    Ok(_) => Ok(Value::Unit),
                    Err(e) => Err(EvalError::ModuleError {
                        message: format!("Failed to append to file '{}': {}", path, e),
                        span: Span::default(),
                    }),
                },
                Err(e) => Err(EvalError::ModuleError {
                    message: format!("Failed to open file '{}': {}", path, e),
                    span: Span::default(),
                }),
            }
        }
        _ => Err(EvalError::TypeError {
            expected: "String, String".to_string(),
            found: format!("{}, {}", args[0].type_name(), args[1].type_name()),
            span: Span::default(),
        }),
    }
}

fn builtin_file_exists(args: &[Value]) -> EvalResult<Value> {
    match &args[0] {
        Value::String(path) => Ok(Value::Bool(std::path::Path::new(path).exists())),
        v => Err(EvalError::TypeError {
            expected: "String".to_string(),
            found: v.type_name(),
            span: Span::default(),
        }),
    }
}
