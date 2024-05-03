use tree_sitter::Node;
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

type SourceLambda = Vec<(u32, u32, u32)>;
type SourceVarRef = Vec<(u32, u32)>;
type SourceAppl = Vec<(u32, u32, u32)>;
struct SourceFacts(SourceLambda, SourceVarRef, SourceAppl);
impl SourceFacts {
    fn from(node: &Node) -> Self {
        // PICKUP
        SourceFacts(
            vec![(6,7,8), (1,2,3)],
            vec![(5,2), (4,2), (10,7), (9,7)],
            vec![(8,9,10), (0,1,6), (3,4,5)])
    }
}

type ValueFlowsTo = Vec<(u32, u32)>;
pub fn value_flows_to(node: &Node) -> ValueFlowsTo {
    let mut dl = AscentProgram::default();
    let facts = SourceFacts::from(node);

    dl.source_lambda = facts.0;
    dl.source_var_ref = facts.1;
    dl.source_application = facts.2;

    dl.run();
    dl.value_flows_to
}
