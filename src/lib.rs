use ascent::ascent;

ascent! {
    relation value_flows_to(u32, u32);

    //       source_lambda(exprId:number, varName:number, bodyId:number)
    relation source_lambda(u32, u32, u32);       // (lambda (xb) ec)a
    //       source_var_ref(exprId:number, varName:number)
    relation source_var_ref(u32, u32);
    relation source_application(u32, u32, u32);  // (e0b e1c)a

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

pub struct LambdaCalculusDl {
    pub source_lambda: Vec<(u32, u32, u32)>,
    pub source_var_ref: Vec<(u32, u32)>,
    pub source_application: Vec<(u32, u32, u32)>
}

impl LambdaCalculusDl {
    pub fn value_flows_to(&self) -> Vec<(u32, u32)> {
        let mut dl = AscentProgram::default();

        dl.source_lambda = self.source_lambda.clone();
        dl.source_var_ref = self.source_var_ref.clone();
        dl.source_application = self.source_application.clone();

        dl.run();
        dl.value_flows_to
    }
}

// ?? source-facts roundtrip: not needed at least for now, only Facts::from(source)
// pre.rkt or its tree_sitter translation

pub struct Facts(pub Vec<(u32, u32, u32)>,pub Vec<(u32, u32)>,pub Vec<(u32, u32, u32)>);
impl Facts {
    pub fn from(_source: impl AsRef<[u8]>) -> Self {
        Facts(
            vec![(6,7,8), (1,2,3)],
            vec![(5,2), (4,2), (10,7), (9,7)],
            vec![(8,9,10), (0,1,6), (3,4,5)])
    }
}

// let mut dl = AscentProgram::default();
// dl.source_lambda = vec![(3,10,2), (6,11,5)];
// dl.source_var_ref = vec![(2,10), (5,11)];
// dl.source_application = vec![(7,3,6)];
// dl.run();
// println!("value_flows_to: {:?}", dl.value_flows_to);  // [(3, 3), (6, 6), (6, 10), (6, 2), (6, 7)]

// let mut d2 = AscentProgram::default();
// d2.source_lambda = vec![(1,2,3), (4,5,6)];
// d2.source_var_ref = vec![(3,2), (6,5)];
// d2.source_application = vec![(0,1,4)];
// d2.run();
// println!("2. value_flows_to: {:?}", d2.value_flows_to);  // [(3, 3), (6, 6), (6, 10), (6, 2), (6, 7)]

// Ω Omega combinator ((lambda (x2) (x4 x5)3)1 (lambda (x7) (x9 x10)8)6)0
// Ω Omega combinator (lambda (x) (x x)) (lambda (x) (x x))
