// Merged PRs using QueryCursor for inspirations:
// https://github.com/zed-industries/zed/pull/11376/files#diff-aa3ab7c2b46d1de04d745789d794c0fb66cfd2fbfa2b18474bde1a7c63d4e2ac

#![allow(unused)]
use std::collections::HashMap;
use tree_sitter::Node;
use ascent::ascent;

ascent! {
    ////////////////////////////////////////////////////////////////////////////
    ////  OUTPUT  /////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////
    relation value_flows_to(u32, u32);  // crate type ValueFlowsTo


    ////////////////////////////////////////////////////////////////////////////
    ////  LANGUAGE  ///////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////

    // (lambda (xb) ec)a
    //                    (exprId:number, varName:number, bodyId:number)
    relation source_lambda(u32,           u32,            u32);


    //                     (exprId:number, varName:number)
    relation source_var_ref(u32,           u32);


    // (e0b e1c)a
    //                         (exprId:number, varName:number, bodyId:number)
    relation source_application(u32, u32, u32);


    ////////////////////////////////////////////////////////////////////////////
    ////  RULES  //////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////

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

// type Table = HashMap<Id, String>
type Var = u32;
type Id = u32;

struct VarIdTable(HashMap<Var, Id>);
impl VarIdTable {
    fn new() -> Self {
        VarIdTable(HashMap::new())
    }
    fn lookup_or_new(&mut self, node: &Node) -> Id {
        12
    }
}

enum Expr {
    VarRef,
    Appl,
    Lambda,
}

type LambdaFact = (u32, u32, u32);
type VarRefFact = (u32, u32);
type ApplFact = (u32, u32, u32);
struct ProgramFacts(Vec<LambdaFact>, Vec<VarRefFact>, Vec<ApplFact>);
impl ProgramFacts {
    fn from(source: impl AsRef<[u8]>) -> Self {
        let mut var_ref_tbl: HashMap<Id, VarRefFact> = HashMap::new();
        let mut appl_tbl: HashMap<Id, ApplFact> = HashMap::new();
        let mut lambda_tbl: HashMap<Id, LambdaFact> = HashMap::new();

        let mut n = 1..;
        let mut var_id_tbl = VarIdTable::new();


        use tree_sitter::{Parser, InputEdit, Point, Node, QueryCursor, Query};

        let lang = &tree_sitter_utlc::language();
        // let lang = &tree_sitter_rust::language();

        // let source = r#"
        // #[allow(unused)]
        // "#;
        // let source = r#"
        // (lambda (x) #f)
        // "#;

        let mut lang_parser = {
            let mut p = Parser::new();
            p.set_language(lang).expect("grammar crate error");
            p
        };

        let mut tree = lang_parser.parse(source, None).expect("todo");

        let mut cursor = QueryCursor::new();

        let mut query = Query::
            new(lang,
                r#"
                (lambda)
                "#)
            .unwrap();

        let text_provider: &[u8] = &[0];
        let matches_iter = cursor
            .matches(&query, tree.root_node(), text_provider);  // ?? what is text_provider used for

        println!("count: {:?}", matches_iter.count());
        // let case_lambda = matches_iter.count() == 1;  // ?? entire source is nothing but lambda
        // for mat in matches_iter {
        //     println!("FORLOOP");
        //     println!("{:?}", mat);
        // }


        // fn make_tables(ast: &Tree) -> Id {
        fn make_tables(expr: &Node) -> Id {
            use Expr::*;
            match expr {
                // VarRef(x) => {
                //     let id = n.next().expect();
                //     let var_id = var_id_tbl.lookup_or_new(x);
                //     var_ref_tbl.insert(id, (id, var_id));
                //     id
                // }
                // Appl(e0, e1) => {
                //     let id = n.next().expect();
                //     let fn_id = make_tables(e0);
                //     let arg_id = make_tables(e1);
                //     appl_tbl.insert(id, fn_id, arg_id);
                //     id
                // }
                // Lambda(x, e_body) => {
                //     let id = n.next().expect();
                //     let var_id = var_id_tbl.lookup_or_new(x);
                //     let body_id = make_tables(e1);
                //     appl_tbl.insert(id, var_id, body_id);
                //     id
                // }
                _ => 0
            }
        }
        let _ = make_tables(&tree.root_node());

        let id = n.next().expect("infinite n");  // ?? closure fn

        let var_id = var_id_tbl.lookup_or_new(&tree.root_node());
        lambda_tbl.insert(id, (id, var_id, id));

        ProgramFacts(
            lambda_tbl.into_values().collect(),
            // vec![(6,7,8), (1,2,3)],
            vec![(5,2), (4,2), (10,7), (9,7)],
            vec![(8,9,10), (0,1,6), (3,4,5)])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::*;

    #[test]
    fn preprocessed_source() {
    }
}

type ValueFlowsTo = Vec<(u32, u32)>;
pub fn value_flows_to(source: impl AsRef<[u8]>) -> ValueFlowsTo {
    let mut dl = AscentProgram::default();

    let facts = ProgramFacts::from(source);

    dl.source_lambda = facts.0;
    dl.source_var_ref = facts.1;
    dl.source_application = facts.2;

    dl.run();
    dl.value_flows_to
}
