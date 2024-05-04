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
    fn from(node: &mut Node) -> Self {
        println!("NODEY: {:?}", node);
        let mut var_ref_tbl: HashMap<Id, VarRefFact> = HashMap::new();
        let mut appl_tbl: HashMap<Id, ApplFact> = HashMap::new();
        let mut lambda_tbl: HashMap<Id, LambdaFact> = HashMap::new();

        let mut n = 1..;
        let mut var_id_tbl = VarIdTable::new();

        // PICKUP tree walk
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
        let _ = make_tables(node);

        let id = n.next().expect("infinite n");  // ?? closure fn

        let var_id = var_id_tbl.lookup_or_new(node);
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
        // ?? method ideas if we ever extend Node: Node::default()
        let mut utlc_parser = {
            let mut p = Parser::new();
            let lang = &tree_sitter_utlc::language();
            p.set_language(lang).expect("grammar crate error");
            p
        };

        let tree = {
            let big_omega = String::from("x");
            // let big_omega = String::from("((lambda (x) (x x)) (lambda (x) (x x)))");
            // let big_omega = String::from("(func a1 a2)");
            utlc_parser.parse(big_omega, None).expect("errors but not panics")
        };
        let _ = ProgramFacts::from(&mut tree.root_node());
    }
}

type ValueFlowsTo = Vec<(u32, u32)>;
pub fn value_flows_to(input: &Node) -> ValueFlowsTo {
    let mut dl = AscentProgram::default();
    let mut prog: Node = input.clone();
    let facts = ProgramFacts::from(&mut prog);

    dl.source_lambda = facts.0;
    dl.source_var_ref = facts.1;
    dl.source_application = facts.2;

    dl.run();
    dl.value_flows_to
}
