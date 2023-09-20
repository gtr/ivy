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
      output.push_str(&print_tuple(node.children, tabs));
    }
    Node::LetExpr(node) => {
      output += &format!("[let{}]\n", if node.is_mut { " mut" } else { "" });
      
      output += &format!("{indent}lhs: ", indent = indent);
      output += &print_tuple_single(node.symbols, tabs + 1);

      if let Some(node2) = &*node.ttype {
        output += &format!("{indent}type: ");
        output += &print_tree_helper(node2, tabs + 1);
      }

      output += &format!("{indent}rhs: ", indent = indent);
      output += &print_tree_helper(&*node.rhs, tabs + 1);
    },
    Node::MutExpr(node) => {
      let mut output = String::new();
      
      output.push_str("[mut]\n");
      
      output += &format!("{indent}lhs: ");
      output += &print_tree_helper(&*node.lhs, tabs + 1);
      output += &format!("{indent}rhs: ");
      output += &print_tree_helper(&*node.rhs, tabs + 1);
    }

    Node::FnAnon(node) => {
      output.push_str("[fn anon]\n");
      output.push_str("{indent}args: ");
      output += &print_tuple_single(node.arguments, tabs + 1);
      
      match *node.type_out {
        Some(node2) => {
          output += &format!("{indent}rtype: ");
          output += &print_tree_helper(&node2, tabs + 1);
        },
        None => {},
      };
      
      output += &format!("{indent}value: ");
      output += &print_tree_helper(&*node.rhs, tabs + 1);
    },
    Node::FnSignature(node) => {
      println!("[fn signature]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.symbol, tabs + 1);
      print!  ("{indent}type: ");
      print_tree_helper(*node.ttype, tabs + 1);
    },
    Node::FnDeclaration(node) => {
      println!("[fn declaration]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.symbol, tabs + 1);
      if node.arguments.len() > 0 {
        print!  ("{indent}args: ");
        print_tuple_single(node.arguments, tabs + 1);
      }

      match *node.type_out {
        Some(node2) => {
            print!  ("{}rtype: ", indent);
            print_tree_helper(node2, tabs + 1);
        }
        None => {}
      }
      print!  ("{indent}value: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::IfExpr(node) => {
      println!("[if]");
      print!  ("{indent}cond: ");
      print_tree_helper(*node.cond, tabs + 1);
      print!  ("{indent}true branch: ");
      print_tree_helper(*node.true_branch, tabs + 1);
      match *node.false_branch {
        Some(node2) => {
            print!  ("{indent}false branch: ");
            print_tree_helper(node2, tabs + 1);
        }
        None => {}
      }
    },
    Node::PubExpr(node) => {
      println!("[pub]");
      print!  ("{indent}rhs: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::DataDeclaration(node) => {
      println!("[data decleration]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.symbol, tabs + 1);
      if node.generics.len() > 0 {
        print!  ("{indent}generics: ");
        print_tuple_single(node.generics, tabs + 1);
      }
      println!("{indent}variants: [tuple]");
      print_tuple(node.variants, tabs + 1);
    },
    Node::DataItem(node) => {
      println!("[data item]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.symbol, tabs + 1);
      print!  ("{indent}type: ");
      print_tree_helper(*node.ttype, tabs + 1);
    },
    Node::StructAnon(node) => {
      println!("[struct anon]");
      println!("{indent}fields: [tuple]");
      print_tuple(node.fields, tabs + 1);
    },
    Node::StructDeclaration(node) => {
      println!("[struct declaration]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.symbol, tabs + 1);
      println!("{indent}fields: [tuple]");
      print_tuple(node.fields, tabs + 1);
    }, 
    Node::StructField(node) => {
      println!("[struct field]");
      print!  ("{indent}name: ");
      print_tree_helper(&*node.symbol, tabs + 1);
      print!  ("{indent}type: ");
      print_tree_helper(&*node.ttype, tabs + 1);
    },
    Node::Package(node) => {
      println!("[package]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::Import(node) => {
      println!("[import]");
      print!  ("{indent}name: ");
      print_tuple_single(node.rhs, tabs + 1);
    },
    Node::MatchExpression(node) => {
      println!("[match]");
      print!  ("{indent}lhs: ");
      print_tree_helper(*node.lhs, tabs + 1);
      println!("{indent}branches: [tuple]");
      print_tuple(node.branches, tabs + 1);
    },
    Node::MatchBranch(node) => {
      println!("[match branch]");
      print!  ("{indent}lhs: ");
      print_tree_helper(*node.lhs, tabs + 1);
      print!  ("{indent}rhs: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::ListLiteral(node) => {
      println!("[list literal]");
      print_tuple(node.items, tabs);
    },
    Node::ListSplit(node) => {
      println!("[list split]");
      print!  ("{}head: ", indent);
      print_tree_helper(*node.head, tabs + 1);
      print!  ("{}tail: ", indent);
      print_tree_helper(*node.tail, tabs + 1);  
    },
    Node::TypeFn(node) => {
      println!("[{}]", node.token.typ);
      print!  ("{indent}lhs: ");
      print_tree_helper(*node.lhs, tabs + 1);
      print!  ("{indent}rhs: ");
      print_tree_helper(*node.rhs, tabs + 1);  
    },
    Node::TypeLst(node) => {
      println!("[list type]");
      print!  ("{indent}type: ");
      print_tree_helper(*node.ttype, tabs + 1);
    },
    Node::TypeTuple(node) => {
      println!("[tuple type]");
      print_tuple(node.ttypes, tabs);
    },
    Node::TypeCmpst(node) => {
      println!("[composite type]");
      print!  ("{indent}name: ");
      print_tree_helper(*node.ttype, tabs + 1);
      print!  ("{}items: ", indent);
      print_tuple_single(node.items, tabs + 1);
    },
    Node::Ttype(node) => {
      if node.is_mut {
        print!("mut ");
      }
      print_tree_helper(*node.symbol, tabs)
    },
    Node::WhileExpression(node) => {
      println!("[while]");
      print!  ("{indent}condition: ");
      print_tree_helper(*node.cond, tabs + 1);
      println!("{indent}stmts: [block]");
      print_tuple(node.statements, tabs + 1);
    },
    Node::DoExpression(node) => {
      println!("[do]");
      println!("{indent}stmts: [block]");
      print_tuple(node.statements, tabs + 1);
    },
    Node::ReturnExpression(node) => {
      println!("[return]");
      print!  ("{indent}value: ");
      print_tree_helper(*node.value, tabs + 1);
    },
    Node::BinaryExpression(node) => {
      println!("[{}]", node.token.typ);
      print!  ("{indent}lhs: ");
      print_tree_helper(*node.lhs, tabs + 1);
      print!  ("{indent}rhs: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::UnaryExpression(node) => {
      println!("[{}]", node.token.typ);
      print!  ("{indent}rhs: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::Call(node) => {
      println!("[call]");
      print!  ("{indent}lhs: ");
      print_tree_helper(*node.lhs, tabs + 1);
      if node.args.len() > 0 {
          print!  ("{}arg: ", indent);
          print_tuple_single(node.args, tabs + 1);
      }
    },
    Node::Access(node) => {
      println!("[access]");
      print!("{indent}lhs: ");
      print_tree_helper(*node.lhs, tabs + 1);
      print!("{indent}rhs: ");
      print_tree_helper(*node.rhs, tabs + 1);
    },
    Node::AccessIndex(node) => {
      println!("[access]");
      print!("{indent}lhs: ");
      print_tree_helper(*node.symbol, tabs + 1);
      print!("{indent}rhs: ");
      print_tree_helper(*node.index, tabs + 1);
    },
    Node::TupleAny(node) => {
      println!("[tuple]");
      if node.items.len() > 0 {
          print_tuple(node.items, tabs);
      }
    }
    Node::Atom(node) => {
      let tok = match node.token.typ {
        TokenType::Symbol(atom)  => format!("Symbol '{}'", atom),
        TokenType::Integer(atom) => format!("Int '{}'", atom),
        TokenType::String(atom)  => format!("String \"{}\"", atom),
        _ => format!(""),
      };
      println!("[{}]", tok);
    },
    Node::FnArgTyped(node) => {
      match *node.ttype {
        None => { print_tree_helper(*node.symbol, tabs); },
        Some(ttype) => {
          println!("[fn argument]");
          print!  ("{indent}symbol: ");
          print_tree_helper(*node.symbol, tabs + 1);
          print!  ("{indent}type: ");
          print_tree_helper(ttype, tabs + 1);
        },
      }
    },
    Node::TraitStmt(node) => {
      println!("[trait definition]");
      print!("{indent}symbol: ");
      print_tree_helper(*node.symbol, tabs + 1);
      println!("{indent}funcs: ");
      print_tuple(node.funcs, tabs + 1);
    },
    Node::ImplStmt(node) => {
      println!("[impl block]");
      print!("{indent}symbol: ");
      print_tree_helper(*node.symbol, tabs + 1);
      print!("{indent}data type: ");
      print_tree_helper(*node.method, tabs + 1);
      println!("{indent}funcs: ");
      print_tuple(node.funcs, tabs + 1);
    }
    _ => output.push_str("-???-\n")
  }
  output
}

fn print_tuple(nodes: Vec<Node>, tabs: usize) -> String {
  let indent = TAB.repeat(tabs);
  let mut output = String::new();
  for (idx, branch) in nodes.iter().enumerate() {
    output.push_str(&format!("{}{}: ", indent, idx));
    output.push_str(&print_tree_helper(branch.clone(), tabs + 1));
  }
  output
}

fn print_tuple_single(nodes: Vec<Node>, tabs: usize) -> String {
  let indent = TAB.repeat(tabs);
  let mut output = String::new();
  if nodes.len() == 0 {
    output.push_str("()");
  } else if nodes.len() > 1 {
    let mut idx = 0;
    output.push_str("[tuple]");
    for arg in nodes {
      output.push_str(&format!("{}{}: ", indent, idx));
      idx += 1;
      print_tree_helper(&arg, tabs + 1);
    }
  } else {
    for arg in nodes {
      print_tree_helper(&arg, tabs + 1);
    }
  }
  output
}
