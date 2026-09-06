use ivy_syntax::decl::{Decl, FnBody};
use ivy_syntax::expr::Expr;
use ivy_syntax::pattern::collect_pattern_names;
use ivy_syntax::Spanned;
use std::collections::{HashMap, HashSet};

fn expr_refs(expr: &Expr, out: &mut HashSet<String>) {
    match expr {
        Expr::Lit(_) => {}
        Expr::Var(id) => {
            out.insert(id.name.clone());
        }
        Expr::Binary { left, right, .. } => {
            expr_refs(&left.node, out);
            expr_refs(&right.node, out);
        }
        Expr::Unary { operand, .. } => expr_refs(&operand.node, out),
        Expr::Let { value, .. } => expr_refs(&value.node, out),
        Expr::Assign { target, value } => {
            expr_refs(&target.node, out);
            expr_refs(&value.node, out);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            expr_refs(&condition.node, out);
            expr_refs(&then_branch.node, out);
            expr_refs(&else_branch.node, out);
        }
        Expr::Match { scrutinee, arms } => {
            expr_refs(&scrutinee.node, out);
            for arm in arms {
                expr_refs(&arm.body.node, out);
            }
        }
        Expr::Lambda { body, .. } => expr_refs(&body.node, out),
        Expr::Call { callee, args } => {
            expr_refs(&callee.node, out);
            for arg in args {
                expr_refs(&arg.node, out);
            }
        }
        Expr::Field { object, .. } => expr_refs(&object.node, out),
        Expr::Index { object, index } => {
            expr_refs(&object.node, out);
            expr_refs(&index.node, out);
        }
        Expr::Do { body } => {
            for stmt in body {
                expr_refs(&stmt.node, out);
            }
        }
        Expr::Tuple { elements } | Expr::List { elements } => {
            for el in elements {
                expr_refs(&el.node, out);
            }
        }
        Expr::Record { fields, .. } => {
            for f in fields {
                expr_refs(&f.value.node, out);
            }
        }
        Expr::RecordUpdate { base, updates } => {
            expr_refs(&base.node, out);
            for f in updates {
                expr_refs(&f.value.node, out);
            }
        }
        Expr::Paren { inner } => expr_refs(&inner.node, out),
    }
}

fn fn_body_refs(body: &FnBody, out: &mut HashSet<String>) {
    match body {
        FnBody::Expr(e) => expr_refs(&e.node, out),
        FnBody::Guards(guards) => {
            for g in guards {
                expr_refs(&g.guard.node, out);
                expr_refs(&g.body.node, out);
            }
        }
    }
}

/// Group the value declarations at `value_idx` into strongly-connected components, returned dependency-first
pub fn order_value_bindings(decls: &[Spanned<Decl>], value_idx: &[usize]) -> Vec<Vec<usize>> {
    let mut node_decls: Vec<Vec<usize>> = Vec::new();
    let mut node_refs: Vec<HashSet<String>> = Vec::new();
    let mut name_to_node: HashMap<String, usize> = HashMap::new();
    let mut fn_node: HashMap<String, usize> = HashMap::new();

    for &i in value_idx {
        match &decls[i].node {
            Decl::Fn(fd) => {
                let nid = *fn_node.entry(fd.name.name.clone()).or_insert_with(|| {
                    node_decls.push(Vec::new());
                    node_refs.push(HashSet::new());
                    node_decls.len() - 1
                });
                node_decls[nid].push(i);
                name_to_node.insert(fd.name.name.clone(), nid);
                fn_body_refs(&fd.body, &mut node_refs[nid]);
            }
            Decl::Let { pattern, value, .. } => {
                let nid = node_decls.len();
                node_decls.push(vec![i]);
                let mut refs = HashSet::new();
                expr_refs(&value.node, &mut refs);
                node_refs.push(refs);
                let mut names = HashSet::new();
                collect_pattern_names(&pattern.node, &mut names);
                for n in names {
                    name_to_node.insert(n, nid);
                }
            }
            Decl::Impl { methods, .. } => {
                node_decls.push(vec![i]);
                let mut refs = HashSet::new();
                for m in methods {
                    fn_body_refs(&m.node.body, &mut refs);
                }
                node_refs.push(refs);
            }
            _ => {}
        }
    }

    let n = node_decls.len();
    let adj: Vec<Vec<usize>> = node_refs
        .iter()
        .map(|refs| {
            let mut targets: Vec<usize> = refs.iter().filter_map(|name| name_to_node.get(name).copied()).collect();
            targets.sort_unstable();
            targets.dedup();
            targets
        })
        .collect();

    let sccs = tarjan_scc(n, &adj);
    sccs.into_iter()
        .map(|mut comp| {
            let mut indices: Vec<usize> = comp
                .drain(..)
                .flat_map(|node| node_decls[node].iter().copied())
                .collect();
            indices.sort_unstable();
            indices
        })
        .collect()
}

/// Tarjan's SCC algorithm: https://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
fn tarjan_scc(n: usize, adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
    struct State<'a> {
        adj: &'a [Vec<usize>],
        index: Vec<Option<u32>>,
        low: Vec<u32>,
        on_stack: Vec<bool>,
        stack: Vec<usize>,
        next: u32,
        out: Vec<Vec<usize>>,
    }

    fn strongconnect(s: &mut State, v: usize) {
        s.index[v] = Some(s.next);
        s.low[v] = s.next;
        s.next += 1;
        s.stack.push(v);
        s.on_stack[v] = true;

        for &w in &s.adj[v] {
            match s.index[w] {
                None => {
                    strongconnect(s, w);
                    s.low[v] = s.low[v].min(s.low[w]);
                }
                Some(idx) if s.on_stack[w] => {
                    s.low[v] = s.low[v].min(idx);
                }
                Some(_) => {}
            }
        }

        if s.low[v] == s.index[v].unwrap() {
            let mut comp = Vec::new();
            loop {
                let w = s.stack.pop().unwrap();
                s.on_stack[w] = false;
                comp.push(w);
                if w == v {
                    break;
                }
            }
            s.out.push(comp);
        }
    }

    let mut state = State {
        adj,
        index: vec![None; n],
        low: vec![0; n],
        on_stack: vec![false; n],
        stack: Vec::new(),
        next: 0,
        out: Vec::new(),
    };
    for v in 0..n {
        if state.index[v].is_none() {
            strongconnect(&mut state, v);
        }
    }
    state.out
}
