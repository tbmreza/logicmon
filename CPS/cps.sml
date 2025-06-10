exception unreachable
exception unimplemented

(* Operations we'd rather not encode from pure lambda calculus. *)
datatype primitive =
  OpOutput
| OpReadFile

(* ??: ast type checking with gadt or phantom types in SML so we can
   do...

   LIf of bool expr * 'a expr * 'a expr

   ...statically here. *)
datatype expr =
  LLet of  (string * expr) * expr
| LInt of  int
| LAdd of  expr * expr
| LMul of  expr list
| LStr of  string
| LVar of  string
| LBool of bool
| LGt of   expr * expr
| LIf of   expr * expr * expr

| LApp of  expr * expr    (* first component reducible to LDef | LPrim *)
| LPrim of primitive      (* LApp (LPrim OpOutput, LVar "x") *)
| LDef of   string * expr  (* fn (string) => expr *)

| LFor_ of string * expr * expr * expr  (* for (string := expr; string <= expr) { body } *)

(* ??: currying list of formal params *)

datatype value =
  UNIT
| INT of    int
| STRING of string
| BOOL of   bool
| REF of    value
(* Closure is a contextualized function definition form.
   env ⊢ fn string => expr  *)
| CLO of string * expr * env

(* type alias wouldn't suffice here in ML; env needs to be a datatype
   with constructor.  *)
and env = Env of ((string * value) list);

fun toString(STRING s) = s
  | toString(INT v) = Int.toString v
  | toString(BOOL v) = Bool.toString v
  | toString(REF v) = toString v
  | toString _ = raise unimplemented

fun doOutput(arg: value) = let
  val _ = print((toString arg) ^ "\n")
in
  UNIT
end

fun doReadFile(arg: value) = let
  val _ = print("unimplemented reading from " ^ (toString arg) ^ "\n")
in
  UNIT
end

(* It matters that env is an ordered list (of history of values)
   that `lookup` returns early on the closest/most local value
   of bound variable.

   Looking up in doesn't pass time in our history. The fact that
   the operation is non-constant time (worst O(n)) is out of our
   granularity.  *)
(* fun lookup x (Env []) = raise Fail ("Unbound variable: " ^ x) *)
(*   | lookup x (Env ((y, v)::env)) = if x = y then v else lookup x (Env env) *)
fun lookup x (Env []) = 
    (print ("DEBUG: Looking up '" ^ x ^ "' in empty environment\n");
     raise Fail ("Unbound variable: " ^ x))
  | lookup x (Env ((y, v)::env)) = 
    (print ("DEBUG: Looking up '" ^ x ^ "', checking against '" ^ y ^ "'\n");
     if x = y then 
       (print ("DEBUG: Found match! Returning value\n"); v)
     else 
       (print ("DEBUG: No match, continuing search...\n");
        lookup x (Env env)))

fun extend x v (Env env) = Env ((x, v)::env)

(* Presumably with `ioInterp : value -> (computation(); UNIT)`,
   halt/non-halting k differentiation is rendered unnecessary. *)
fun ioInterp (INT v) =    (v; UNIT)
  | ioInterp (STRING v) = (v; UNIT)
  | ioInterp (BOOL v) =   (v; UNIT)
  | ioInterp  UNIT =     ((); UNIT)

  | ioInterp (CLO _) =
    (print("<closure>\n");
     UNIT)

  | ioInterp (REF v) = ioInterp v

fun node (BOOL b) = LBool b
  | node (CLO (farg, exp, ctx)) = LDef (farg, exp)
  (* | node  = *)
  | node _ = raise unimplemented

fun unref (INT n) = LInt n
  | unref (REF r) = unref r
  | unref _ = raise unreachable


datatype snapshot =
  Snapshot of { ssExpr: expr
              , ssEnv: env
              , ssCont: value -> value
              }

(* ??: visualize to web *)
val mutHistory : (snapshot list) ref
               = ref []

fun snap! ssExpr ssEnv ssCont = let
  val ss = Snapshot
    { ssExpr = ssExpr
    , ssEnv = ssEnv
    , ssCont = ssCont
    }
  val _ = mutHistory := ss :: !mutHistory
in
  ssCont
end

(* Evaluate AST to a value via CPS rules. Save env on every interesting
   reductions (i.e. cps invocations in rhs' that extends env, ??: branches that we take, ).

    cps :     expr    -> value  *)
fun cps (LInt v, _, k) = k (INT v)
  | cps (LStr v, _, k) = k (STRING v)
  | cps (LBool v, _, k) = k (BOOL v)

  | cps (LDef (farg, body), env, k) =
    k (CLO (farg, body, env))

  | cps (LVar var, env, k) = let val var' = lookup var env in
    k (REF var') end


  | cps (LAdd (LInt m, LInt n), env, k) =
    k (INT (m + n))

  | cps (LAdd (la, lb), env, k) =
    cps (la, env, fn la' =>
      cps (lb, env, fn lb' =>
        cps (LAdd (unref la', unref lb'), env, k)))


  | cps (LMul [LInt m, LInt n], env, k) =
    k (INT (m * n))

  | cps (LMul [la, lb], env, k) =
    cps (la, env, fn la' =>
      cps (lb, env, fn lb' =>
        cps (LMul [(unref la'), (unref lb')], env, k)))


  | cps (LApp (LDef (farg, exp), arg), env, k) =
    (* Closure's env is extended with (farg, arg'). *)
    cps (arg, env, fn arg' => let val env' = (extend farg arg' env) in
      cps (exp, env', k) end)

  (* | cps (LApp (CLO (farg, exp, closed), arg), env, k) = *)
  (*   let *)
  (*     val _ = 0 *)
  (*     (* val ctx = (extend farg arg' closed) *) *)
  (*   in *)
  (*     cps (LApp (LDef (farg, exp), arg), closed, k) *)
  (*   end *)

  | cps (LApp (LPrim prim, exp), env, k) =
    cps (exp, env, fn v => k (case prim of
      OpOutput => doOutput v
    | OpReadFile => doReadFile v))

  | cps (LApp (f, arg), env, k) =
    cps (f, env, fn f' =>
      cps (arg, env, fn arg' =>
        cps (LApp (node f', node arg'), env, k)))


  | cps (LIf (LBool true, exp, _), env, k) =
    cps (exp, env, fn exp' => k exp')

  | cps (LIf (LBool false, _, exp), env, k) =
    cps (exp, env, fn exp' => k exp')

  | cps (LIf (b, thn, els), env, k) =
    cps (b, env, fn b' =>
      cps (LIf (node b', thn, els), env, k))

(*
  for i = lo to hi do
    body(i)

  ... ??: for-loop desugars to a let-form ...

  let
    fun loop i =
      if i > hi then ()
      else (
        body(i);
        loop (i + 1)
      )
  in
    loop lo
  end
*)
  
  (*
  a := 11         -- env { a: 11 }
  b := a + 100    -- env { a: 11, b: 111 }
  pi := 300 + 14  -- env { pi: 314 }
  ()  -- unit body
  *)

  | cps (LLet ((name, exp), body), env, k) =
    cps (exp, env, fn exp' => let val env' = (extend name exp' env) in
      cps (body, env', snap! body env' k) end)


  | cps (LGt (LInt m, LInt n), env, k) =
    k (BOOL (m > n))

  | cps (LGt (a, b), env, k) =
    cps (a, env, fn a' =>
      cps (b, env, fn b' => cps (LGt (node a', node b'), env, k)))

  (* | cps (LFor_ (loopVar, LInt lo, LInt hi, body), env, k) = *)
  (* ??: Wrap body with a lambda for let-form desugaring. *)

    (* | cps (LFor_ (loopVar, lo, hi, body), env, k) = *)

  (* | cps (todo, _, _) = (print("todo"); raise unimplemented) *)

fun stmt1(k) = k(print("stmt1"));
fun stmt2(k) = k(print("stmtB"));

fun put(arg, k) =
  k(print(arg));

fun println(arg, k) =
  put(arg, fn _ => k(print("\n")));

fun eq(x, y, k) = k(x = y);

fun letin(str, k) = k(str);

fun script(ast) = let
  val builtins = Env
    [ ("ident", CLO ("x", LVar "x", Env []))
    , ("#output", CLO ("x", LApp (LPrim OpOutput, LVar "x"), Env []))
    , ("version", STRING "v1.0")
    ]

  val _ = ioInterp
    (cps (ast, builtins, snap! ast builtins (fn v => v)))

in () end;

val mt = Env []
fun void(k) = let
  (* val ast1 = LApp ((LVar "#output"), (LStr "something good happened !!!!")) *)
  (* val _ = ioInterp (cps (ast1, mt, k)) *)
  (**)
  (* val ast3 = LApp ((LVar "#output"), LMul [LInt 2, LInt 9]) *)
  (* val _ = ioInterp (cps (ast3, Env [("Kilo", (INT 1000))], k)) *)
  (**)
  (* val ast2 = LApp ((LVar "#output"), (LVar "pi")) *)
  (* val _ = ioInterp (cps (ast2, Env [("pi", (INT 415))], k)) *)
  (**)
  (* val ast = LApp ((LVar "#output"), LMul [LInt 2, (LVar "Kilo")]) *)
  (* val _ = ioInterp (cps (ast, Env [("Kilo", (INT 1000))], k)) *)

  val ast4 =
    LLet (("user", LStr "09"),
      LApp ((LPrim OpOutput), (LVar "user")))
  val _ = ioInterp (cps (ast4, mt, k))

  val ast5 =
    LLet (("user", LStr "08"), LLet (("user", LStr "07"), LApp ((LPrim OpOutput), (LVar "user"))))
  val _ = ioInterp (cps (ast5, mt, k))

  val ast6 =
    LLet (("user", LStr "3..."),
      LLet (("user", LStr "2.."),
        LLet (("user", LStr "1."), LApp ((LPrim OpOutput), (LVar "user")))))
  val _ = ioInterp (cps (ast6, mt, k))

  val ast7 =
    LIf ((LBool true), (LApp ((LPrim OpOutput), (LStr " tru branch !!!!"))), (LApp ((LVar "#output"), (LStr "fals branch !!!!"))))
  val _ = ioInterp (cps (ast7, mt, k))


(* time travel for 
   [("g", (INT 10))]
     g := 8
     g := 5
     z := 100
     #output g
 *)

  in
    print("")
  end;
void(fn v => v);

(* fun seq(stmt1, stmt2, k) = *)
(*   stmt1(fn _ => *)
(*     stmt2(k)); *)

datatype fnValueOrExpr =
  Fn of expr
| LFn of value;

fun apply (Fn f, arg) = LApp (f, arg)
  | apply (LFn (CLO (farg, body, _)), arg) = LApp (LDef (farg, body), arg)
  | apply _ = raise unreachable;

script(apply (Fn (LPrim OpOutput), LVar "version"));
script(apply (LFn (CLO ("x", LApp (LPrim OpOutput, LVar "x"), Env [])), LStr "yippi"));
(* script(apply (Fn (LVar "#output"), LStr "\tplease")); *)


(* ??: Recursion that prints every time infinitely. *)
(* let f = CLO ("_", LApp (LVar "f", LInt 0), env) in LApp (LVar f, LInt 0) *)

(* script(LFor_ ("_", LInt 0, LInt 4, LApp (LPrim OpOutput, LStr "\tsomething\n"))); *)

(* script( *)
(*   LApp( *)
(*     (LVar "loop"), *)
(*     LAdd (LVar "i", LInt 1) *)
(*   ) *)
(* ); *)
