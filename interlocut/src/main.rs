#![allow(unused)]
use tree_sitter::{Parser, InputEdit, Point, Node};

/// Model of text editor that runs in the browser.
type NewSource = String;
fn editor() -> (NewSource, InputEdit) {
    // let new_src = String::from("fn test(sangatpanjaa: u32) {}");
    let new_src = String::from("(func a1 a2 a3)");

    // let ie = InputEdit {
    //     start_byte: 8,
    //     old_end_byte: 8,
    //     new_end_byte: 14,
    //     start_position: Point::new(0, 8),
    //     old_end_position: Point::new(0, 8),
    //     new_end_position: Point::new(0, 14),
    // };
    let ie = InputEdit {
        start_byte: 12,
        old_end_byte: 12,
        new_end_byte: 18,
        start_position: Point::new(0, 12),
        old_end_position: Point::new(0, 12),
        new_end_position: Point::new(0, 18),
    };

    (new_src, ie)
}

type StdOut = String;
enum InternalError {
    Todo
}

/// Model of things being sent back to browser.
fn goodies() -> (StdOut, Vec<InternalError>) {
    let mut rust_parser = {
        let mut p = Parser::new();
        // let lang = &tree_sitter_rust::language();
        let lang = &tree_sitter_utlc::language();
        p.set_language(lang).expect("grammar crate error");
        p
    };

    let mut tree = {
        // let big_omega = String::from("fn test() {}");  // ??
        // let big_omega = String::from("((lambda (x) (x x)) (lambda (x) (x x)))");
        let big_omega = String::from("(func a1 a2)");
        rust_parser.parse(big_omega, None).expect("?? can source make this panic")
    };
    println!("{:?}", tree.root_node());
    let tree_clone = tree.clone();
    let mut cursor = tree_clone.walk();
    cursor.goto_first_child();
    let ss = cursor.node().to_string();
    println!("{:?}", ss);

    // rerun ascent on edit
    let (new_src, coords) = editor();
    tree.edit(&coords);
    let new_tree = rust_parser.parse(new_src, Some(&tree)).expect("todo");
    let nt = new_tree.root_node();
    println!("{:?}", nt);

    let stdout = format!("{:?}", analyze_value_flows_to(&nt));
    (stdout, Vec::new())
}

type SourceLambda = Vec<(u32, u32, u32)>;
type SourceVarRef = Vec<(u32, u32)>;
type SourceAppl = Vec<(u32, u32, u32)>;
struct SourceFacts(SourceLambda, SourceVarRef, SourceAppl);
impl SourceFacts {
    fn from(_node: &Node) -> Self {
        SourceFacts(
            vec![(6,7,8), (1,2,3)],
            vec![(5,2), (4,2), (10,7), (9,7)],
            vec![(8,9,10), (0,1,6), (3,4,5)])
    }
}

use ascent::ascent;

ascent! {
    ////////////////////////////////////////////////////////////////////////////
    ////  OUTPUT  //////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    relation value_flows_to(u32, u32);  // crate type ValueFlowsTo


    ////////////////////////////////////////////////////////////////////////////
    ////  LANGUAGE  ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    // (lambda (xb) ec)a
    //                    (exprId:number, varName:number, bodyId:number)
    relation source_lambda(u32,           u32,            u32);


    //                     (exprId:number, varName:number)
    relation source_var_ref(u32,           u32);


    // (e0b e1c)a
    //                         (exprId:number, varName:number, bodyId:number)
    relation source_application(u32, u32, u32);


    ////////////////////////////////////////////////////////////////////////////
    ////  RULES  ///////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    // base: atom flows to itself
    value_flows_to(expr, expr) <-- source_lambda(expr, _, _);

    // calls: value flows to bound argument
    value_flows_to(arg_val, x) <-- source_application(_, fun, arg),
                                   value_flows_to(_, arg),
                                   source_lambda(fun, x, _),
                                   value_flows_to(arg_val, arg);

    // returns: returning means value flowing to var ref or lambda body
    value_flows_to(v, expr) <-- source_var_ref(expr, var),
                                value_flows_to(v, var);
    value_flows_to(v, expr) <-- source_application(expr, fun, _),
                                value_flows_to(lam, fun),
                                source_lambda(lam, _, body),
                                value_flows_to(v, body);
}

type ValueFlowsTo = Vec<(u32, u32)>;
fn analyze_value_flows_to(node: &Node) -> ValueFlowsTo {
    let mut dl = AscentProgram::default();
    let facts = SourceFacts::from(node);

    dl.source_lambda = facts.0;
    dl.source_var_ref = facts.1;
    dl.source_application = facts.2;

    dl.run();
    dl.value_flows_to
}

fn main() {
    let (browser_std_out, browser_internal_errors) = goodies();
    println!("browser: {:?}", browser_std_out);
}
