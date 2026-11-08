use std::collections::HashMap;

use super::matrix::{contains_record, row_count, rows, useful, MAX_ROWS};
use crate::error::TypeWarning;
use crate::registry::TypeRegistry;
use ivy_syntax::decl::{Decl, FnDecl};
use ivy_syntax::pattern::Pattern;
use ivy_syntax::{Param, Spanned};

fn columns(params: &[Param]) -> Vec<&Pattern> {
    params.iter().map(|p| &p.pattern.node).collect()
}

pub fn unreachable_clause_warnings(decls: &[Spanned<Decl>], registry: &TypeRegistry) -> Vec<TypeWarning> {
    let mut groups: HashMap<&str, Vec<&FnDecl>> = HashMap::new();
    let mut order: Vec<&str> = Vec::new();
    for decl in decls {
        if let Decl::Fn(fn_decl) = &decl.node {
            let name = fn_decl.name.name.as_str();
            if !groups.contains_key(name) {
                order.push(name);
            }
            groups.entry(name).or_default().push(fn_decl);
        }
    }

    let mut warnings = Vec::new();
    for name in order {
        let clauses = &groups[name];
        let width = clauses[0].params.len();
        if clauses.len() < 2 || clauses.iter().any(|c| c.params.len() != width) {
            continue;
        }
        if clauses
            .iter()
            .any(|c| c.params.iter().any(|p| contains_record(&p.pattern.node)))
        {
            continue;
        }
        let total_rows = clauses
            .iter()
            .map(|c| row_count(&columns(&c.params)))
            .fold(0, usize::saturating_add);
        if total_rows > MAX_ROWS {
            continue;
        }

        let mut matrix: Vec<Vec<Pattern>> = Vec::new();
        for (index, clause) in clauses.iter().enumerate() {
            let clause_rows = rows(&columns(&clause.params));
            let reachable = clause_rows.iter().any(|row| useful(&matrix, row, registry));
            if !reachable {
                // point at a single earlier clause when one alone subsumes this one (the common catchall case)
                let covered_by = clauses[..index]
                    .iter()
                    .rev()
                    .find(|earlier| {
                        let earlier_rows = rows(&columns(&earlier.params));
                        clause_rows.iter().all(|row| !useful(&earlier_rows, row, registry))
                    })
                    .map(|earlier| earlier.span);
                warnings.push(TypeWarning::UnreachableClause {
                    name: name.to_string(),
                    span: clause.span,
                    covered_by,
                });
            }
            matrix.extend(clause_rows);
        }
    }
    warnings
}

#[cfg(test)]
mod tests {
    use super::*;

    fn warnings(code: &str) -> Vec<TypeWarning> {
        let program = ivy_parse::parse(code).expect("parse failed");
        unreachable_clause_warnings(&program.declarations, &TypeRegistry::with_builtins())
    }

    #[test]
    fn test_warning_points_at_covering_clause() {
        let code = "fn f('x') => 1;\nfn f(_: Char) => 0;\nfn f('y') => 2;";
        let found = warnings(code);
        assert_eq!(found.len(), 1);
        let TypeWarning::UnreachableClause { span, covered_by, .. } = &found[0];
        assert_eq!(&code[span.start..span.end], "fn f('y') => 2;");
        let covered_by = covered_by.expect("a single clause covers it");
        assert_eq!(&code[covered_by.start..covered_by.end], "fn f(_: Char) => 0;");
    }

    #[test]
    fn test_wildcard_before_literal_is_unreachable() {
        assert_eq!(warnings("fn f(_: Char) => 0;\nfn f('x') => 1;").len(), 1);
    }

    #[test]
    fn test_literal_before_wildcard_is_reachable() {
        assert!(warnings("fn f('x') => 1;\nfn f(_: Char) => 0;").is_empty());
    }

    #[test]
    fn test_catch_all_before_specific_second_arg() {
        assert_eq!(
            warnings("fn step(s: Int, _: Char) => s;\nfn step(s: Int, ']') => s;").len(),
            1
        );
    }

    #[test]
    fn test_interleaved_definitions_grouped_by_name() {
        let code = "fn f(_: Char) => 0;\nfn g(n: Int) => n;\nfn f('x') => 1;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_bool_true_false_shadow_wildcard() {
        let code = "fn f(true) => 1;\nfn f(false) => 2;\nfn f(_) => 3;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_option_ctors_shadow_wildcard() {
        let code = "fn f(None) => 0;\nfn f(Some(_)) => 1;\nfn f(_) => 2;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_list_nil_cons_shadow_wildcard() {
        let code = "fn f([]) => 0;\nfn f([_ | _]) => 1;\nfn f(_) => 2;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_incomplete_bool_wildcard_reachable() {
        let code = "fn f(true) => 1;\nfn f(_) => 2;";
        assert!(warnings(code).is_empty());
    }

    #[test]
    fn test_infinite_literals_wildcard_reachable() {
        let code = "fn f(0) => 0;\nfn f(1) => 1;\nfn f(_) => 2;";
        assert!(warnings(code).is_empty());
    }

    #[test]
    fn test_variable_before_literal_is_unreachable() {
        let code = "fn f(x) => x;\nfn f(0) => 0;";
        assert_eq!(warnings(code).len(), 1);
    }

    #[test]
    fn test_record_patterns_skip_analysis() {
        let code = "type P = { x: Int };\nfn f(P { x: 0 }) => 0;\nfn f(P { x: _ }) => 1;";
        assert!(warnings(code).is_empty());
    }

    #[test]
    fn test_or_pattern_blowup_is_skipped() {
        let small = ["true|false"; 3].join(", ");
        assert_eq!(warnings(&format!("fn f(({small})) => 0;\nfn f(t) => 1;")).len(), 1);

        let positions = ["true|false"; 22].join(", ");
        let code = format!("fn f(({positions})) => 0;\nfn f(t) => 1;");
        assert!(warnings(&code).is_empty());
    }

    #[test]
    fn test_small_or_product_still_analyzed() {
        let code = "fn f(1|2, _) => 0;\nfn f(3, _) => 1;\nfn f(2, true) => 2;";
        let found = warnings(code);
        assert_eq!(found.len(), 1);
        let TypeWarning::UnreachableClause { span, .. } = &found[0];
        assert_eq!(&code[span.start..span.end], "fn f(2, true) => 2;");
    }
}
