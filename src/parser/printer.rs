use crate::lexer::tokens::*;
use crate::parser::ast::*;

const TAB: &str = "  ";

pub fn print_tree(node: Node) {
  let output = print_tree_helper(&node, 1);
  println!("{}", output);
}

fn print_tree_helper(node: &Node, tabs: usize) -> String {
  let indent = TAB.repeat(tabs);
  let mut output = String::new();
  
  match node {
    Node::Root(node) => {
      output.push_str("\n[root]\n");
      output.push_str(&print_tuple(&node.children, tabs));
    }
    Node::LetExpr(node) => {
      output += &format!("[let{}]\n", if node.is_mut { " mut" } else { "" });
      
      output += &format!("{indent}lhs: ", indent = indent);
      output += &print_tuple_single(&node.symbols, tabs + 1);

      if let Some(node2) = &*node.ttype {
        output += &format!("{indent}type: ");
        output += &print_tree_helper(node2, tabs + 1);
      }

      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&*node.rhs, tabs + 1);
    },
    Node::MutExpr(node) => {
      output.push_str("[mut]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&*node.lhs, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&*node.rhs, tabs + 1);
    }
    Node::FnAnon(node) => {
      output.push_str("[fn anon]\n");
      output.push_str("{indent}args: ");
      output += &print_tuple_single(&node.arguments, tabs + 1);
      
      match &node.type_out {
        Some(node2) => {
          output += &format!("{indent}rtype: ");
          output += &print_tree_helper(&**node2, tabs + 1);
        },
        None => {},
    };
      
      output += &format!("{indent}value: ");
      output += &print_tree_helper(&*node.rhs, tabs + 1);
    },
    Node::FnSignature(node) => {
      output.push_str("[fn signature]\n");

      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}type: ");
      output += &print_tree_helper(&node.ttype, tabs + 1);
    },
    Node::FnDeclaration(node) => {
      output.push_str("[fn declaration]\n");
      
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      
      if node.arguments.len() > 0 {
        output += &format!("{indent}args: ");
        output += &print_tuple_single(&node.arguments, tabs + 1);
      }

      match &node.type_out {
        Some(node2) => {
          output += &format!("{indent}rtype: ");
          output += &print_tree_helper(&node2, tabs + 1);
        }
        None => {}
      }

      output += &format!("{indent}value: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::IfExpr(node) => {
      output.push_str("[if]\n");
      
      output += &format!("{indent}cond: ");
      output += &print_tree_helper(&node.cond, tabs + 1);
      output += &format!("{indent}true: ");
      output += &print_tree_helper(&node.true_branch, tabs + 1);
      
      match &node.false_branch {
        Some(node2) => {
          output += &format!("{indent}false: ");
          output += &print_tree_helper(&node2, tabs + 1);
        }
        None => {}
      }
    },
    Node::PubExpr(node) => {
      output.push_str("[pub]\n");
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::DataDeclaration(node) => {
      output.push_str("[data declaration]\n");
      
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      
      if node.generics.len() > 0 {
        output += &format!("{indent}generics: ");
        output += &print_tuple_single(&node.generics, tabs + 1);
      }
      
      output += &format!("{indent}variants: [tuple] \n");
      output += &print_tuple(&node.variants, tabs + 1);
    },
    Node::DataItem(node) => {
      output.push_str("[data item]\n");
      
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}type: ");
      output += &print_tree_helper(&node.ttype, tabs + 1);
    },
    Node::StructAnon(node) => {
      output.push_str("[struct anon]\n");
      output += &format!("{indent}fields: [tuple]\n");
      output += &print_tuple(&node.fields, tabs + 1);
    },
    Node::StructDeclaration(node) => {
      output.push_str("[struct declaration]\n");
      
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}fields [tuple]:\n");
      output += &print_tuple(&node.fields, tabs + 1);
    }, 
    Node::StructField(node) => {
      output.push_str("[struct field]\n");
      
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}type: ");
      output += &print_tree_helper(&node.ttype, tabs + 1);
    },
    Node::Package(node) => {
      output.push_str("[package]\n");
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::Import(node) => {
      output.push_str("[import]\n");
      output += &format!("{indent}name: ");
      output += &print_tuple_single(&node.rhs, tabs + 1);
    },
    Node::MatchExpression(node) => {
      output.push_str("[match]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.lhs, tabs + 1);
      output += &format!("{indent}branches [tuple]: ");
      output += &print_tuple(&node.branches, tabs + 1);
    },
    Node::MatchBranch(node) => {
      output.push_str("[match branch]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.lhs, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::ListLiteral(node) => {
      output.push_str("[list literal]\n");
      output += &print_tuple(&node.items, tabs + 1);
    },
    Node::ListSplit(node) => {
      output.push_str("[list split]\n");

      output += &format!("{indent}head: ");
      output += &print_tree_helper(&node.head, tabs + 1);
      output += &format!("{indent}tail: ");
      output += &print_tree_helper(&node.tail, tabs + 1);
    },
    Node::TypeFn(node) => {
      output += &format!("[{}]:\n", node.token.typ);
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.lhs, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::TypeLst(node) => {
      output.push_str("[list type]\n");
      output += &format!("{indent}type: ");
      output += &print_tree_helper(&node.ttype, tabs + 1);
    },
    Node::TypeTuple(node) => {
      output.push_str("[tuple type]\n");
      output += &print_tuple(&node.ttypes, tabs + 1);
    },
    Node::TypeCmpst(node) => {
      output.push_str("[composite type]\n");
      
      output += &format!("{indent}name: ");
      output += &print_tree_helper(&node.ttype, tabs + 1);
      output += &format!("{indent}items: ");
      output += &print_tuple_single(&node.items, tabs + 1);
    },
    Node::Ttype(node) => {
      if node.is_mut {
        output += &format!("mut: ");
      }
      output += &print_tree_helper(&node.symbol, tabs);
    },
    Node::WhileExpression(node) => {
      output.push_str("[while]\n");

      output += &format!("{indent}condition: ");
      output += &print_tree_helper(&node.cond, tabs + 1);
      output += &format!("{indent}stmts: [block]");
      output += &print_tuple(&node.statements, tabs + 1);
    },
    Node::DoExpression(node) => {
      output.push_str("[do]\n");
      output += &format!("{indent}stmts: [block]");
      output += &print_tuple(&node.statements, tabs + 1);
    },
    Node::ReturnExpression(node) => {
      output.push_str("[return]\n");
      output += &format!("{indent}value: ");
      output += &print_tree_helper(&node.value, tabs + 1);
    },
    Node::BinaryExpression(node) => {
      output += &format!("[{}] \n", node.token.typ);
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.lhs, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::UnaryExpression(node) => {
      output += &format!("[{}] \n", node.token.typ);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::Call(node) => {
      output.push_str("[call]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.lhs, tabs + 1);
      
      if node.args.len() > 0 {
        output += &format!("{indent}arg: ");
        output += &print_tuple_single(&node.args, tabs + 1);
      }
    },
    Node::Access(node) => {
      output.push_str("[access]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.lhs, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.rhs, tabs + 1);
    },
    Node::AccessIndex(node) => {
      output.push_str("[access]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&node.index, tabs + 1);
    },
    Node::TupleAny(node) => {
      output.push_str("[tuple]\n");
      if node.items.len() > 0 {
        output += &print_tuple(&node.items, tabs);
      }
    }
    Node::Atom(node) => {
      let tok = match &node.token.typ {
        TokenType::Symbol(atom)  => format!("[Symbol '{}']\n", atom),
        TokenType::Integer(atom) => format!("[Int '{}']\n", atom),
        TokenType::String(atom)  => format!("[String \"{}\"]\n", atom),
        _ => format!(""),
      };
      output += &tok;
    },
    Node::FnArgTyped(node) => {
      match &node.ttype {
        None => { output += &print_tree_helper(&node.symbol, tabs + 1); },
        Some(ttype) => {
          output.push_str("[tuple]\n");

          output += &format!("{indent}symbol: ");
          output += &print_tree_helper(&node.symbol, tabs + 1);
          output += &format!("{indent}type: ");
          output += &print_tree_helper(&ttype, tabs + 1);
        },
      }
    },
    Node::TraitStmt(node) => {
      output.push_str("[trait definition]\n");

      output += &format!("{indent}symbol: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}funcs: \n");
      output += &print_tuple(&node.funcs, tabs + 1);
    },
    Node::ImplStmt(node) => {
      output.push_str("[impl block]\n");
      
      output += &format!("{indent}symbol: ");
      output += &print_tree_helper(&node.symbol, tabs + 1);
      output += &format!("{indent}data type: ");
      output += &print_tree_helper(&node.method, tabs + 1);
      output += &format!("{indent}funcs: \n");
      output += &print_tuple(&node.funcs, tabs + 1);
    }
    _ => output.push_str("-???-\n")
  }
  output
}

fn print_tuple(nodes: &Vec<Node>, tabs: usize) -> String {
  let indent = TAB.repeat(tabs);
  let mut output = String::new();
  for (idx, branch) in nodes.iter().enumerate() {
    output.push_str(&format!("{}{}: ", indent, idx));
    output.push_str(&print_tree_helper(branch.clone(), tabs + 1));
  }
  output
}

fn print_tuple_single(nodes: &Vec<Node>, tabs: usize) -> String {
  let indent = TAB.repeat(tabs);
  let mut output = String::new();
  if nodes.len() == 0 {
    output.push_str("()");
  } else if nodes.len() > 1 {
    let mut idx = 0;
    output.push_str("[tuple]\n");
    for arg in nodes {
      output.push_str(&format!("{}{}: ", indent, idx));
      idx += 1;
      output += &print_tree_helper(&arg, tabs + 1);
    }
  } else {
    for arg in nodes {
      output += &print_tree_helper(&arg, tabs + 1);
    }
  }
  output
}
