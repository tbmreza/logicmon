use "../CPS/cps.sml"; open Poster; open Types;

script(apply (LPrim OpOutput, LVar "version"));
script(apply (LFn (CLO ("x", LApp (LPrim OpOutput, LVar "x"), Env [])), LStr "yippi"));

script(apply (LVar "#output", LStr "\tplease"));

script(LProg (apply (LVar "#output", LStr "\tfst"), apply (LVar "#output", LStr "\tsnd")));

script(
  LLet (
    ("uprint", LDef ("x", LApp (LPrim OpOutput, LVar "x"))),
    apply (LVar "uprint", LStr "\tyattayooo"))
);


script(
LProg (
apply (LVar "#output", LStr "\tfst"), LProg (
apply (LVar "#output", LStr "\tsnd"),
apply (LVar "#output", LStr "\tthd")))
);

(*

a := 11
a := 22
a := 33
#output a

*)
