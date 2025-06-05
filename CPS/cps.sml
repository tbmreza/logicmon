exception unreachable
exception unimplemented

(* Abstract syntax

*)
datatype expr =
  LLet of  (string * expr) * expr
| LInt of  int
| LMul of  expr list
| LStr of  string
| LBool of bool
| LApp of  expr * expr
| LVar of  string
| LIf of   expr * expr * expr

datatype value =
  INT of int
| STRING of string
| BOOL of bool
| OUTPUT of string
| REF of value
| UNIT

type env = (string * value) list;

(* It matters that env is an ordered list (of history of values)
   that `lookup` returns early on the closest/most local value
   of bound variable. *)
fun lookup x [] = raise Fail ("Unbound variable: " ^ x)
  | lookup x ((y, v)::env) = if x = y then v else lookup x env

fun extend x v env = (x, v)::env

(* Presumably with `ioInterp : value -> (computation(); UNIT)`,
   halt/non-halting k differentiation is rendered unnecessary. *)
fun ioInterp (INT v) =    (v; UNIT)
  | ioInterp (STRING v) = (v; UNIT)
  | ioInterp (BOOL v) =   (v; UNIT)
  | ioInterp  UNIT =     ((); UNIT)

  | ioInterp (OUTPUT s) =
    (print(s ^ "\n");
     UNIT)

  | ioInterp (REF v) = ioInterp v

fun toString(STRING s) = s
  | toString(INT v) = Int.toString v
  | toString(BOOL v) = Bool.toString v
  | toString(REF v) = toString v
  | toString _ = raise unimplemented

fun asExpr (BOOL b) = (LBool b)
  | asExpr _ = raise unimplemented

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
   reductions (??: cps invocations in rhs' that extends env?).

    cps :     expr    -> value  *)
fun cps (LInt v, _, k) = k (INT v)
  | cps (LStr v, _, k) = k (STRING v)
  | cps (LBool v, _, k) = k (BOOL v)

  | cps (LVar name, env, k) = let
    val got = lookup name env
  in
    k (REF got)
  end


  | cps (LMul [(LInt m), (LInt n)], env, k) =
    k (INT (m * n))

  | cps (LMul [la, lb], env, k) =
    cps (la, env, fn la' =>
      cps (lb, env, fn lb' =>
        cps (LMul [(unref la'), (unref lb')], env, k)))


  | cps (LApp ((LVar "#output"), arg), env, k) =
    cps (arg, env, fn got => k (OUTPUT (toString got)))


  | cps (LIf ((LBool true), exp, _), env, k) =
    cps (exp, env, fn exp' => k exp')

  | cps (LIf ((LBool false), _, exp), env, k) =
    cps (exp, env, fn exp' => k exp')

  | cps (LIf (b, thn, els), env, k) =
    cps (b, env, fn b' =>
      cps (LIf (asExpr b', thn, els), env, k))

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

  | cps _ = raise unimplemented

fun stmt1(k) = k(print("stmt1"));
fun stmt2(k) = k(print("stmtB"));

fun put(arg, k) =
  k(print(arg));

fun println(arg, k) =
  put(arg, fn _ => k(print("\n")));

fun eq(x, y, k) = k(x = y);

fun letin(str, k) = k(str);

fun kInit!(e: env) =
  let
    val _ = 0
    (* val _ = SNAPSHOT := SOME e *)
  in
    fn v => v
  end

fun script(ast) = let
  val builtins = []

  val _ = ioInterp
    (cps (ast, builtins, snap! ast builtins (fn v => v)))

in () end;
script(LApp ((LVar "#output"), (LStr "something awsm happened !!!!")));

(*
  "time travel"

  type state = cont
  , trace : , cps :  -> [state]
  env_from_cont : fn -> dict<name, expr/value>
  rewind : _::[state] -> [state]
  goto : [state] -> index -> state

  *)
fun void(k) = let
  val ast1 = LApp ((LVar "#output"), (LStr "something good happened !!!!"))
  val _ = ioInterp (cps (ast1, [], k))

  val ast3 = LApp ((LVar "#output"), LMul [LInt 2, LInt 9])
  val _ = ioInterp (cps (ast3, [("Kilo", (INT 1000))], k))

  val ast2 = LApp ((LVar "#output"), (LVar "pi"))
  val _ = ioInterp (cps (ast2, [("pi", (INT 415))], k))

  val ast = LApp ((LVar "#output"), LMul [LInt 2, (LVar "Kilo")])
  val _ = ioInterp (cps (ast, [("Kilo", (INT 1000))], k))

  val ast4 =
    LLet (("user", LStr "09"),
      LApp ((LVar "#output"), (LVar "user")))
  val _ = ioInterp (cps (ast4, [], k))

  val ast5 =
    LLet (("user", LStr "08"), LLet (("user", LStr "07"), LApp ((LVar "#output"), (LVar "user"))))
  val _ = ioInterp (cps (ast5, [], k))

  val ast6 =
    LLet (("user", LStr "3..."),
      LLet (("user", LStr "2.."),
        LLet (("user", LStr "1."), LApp ((LVar "#output"), (LVar "user")))))
  val _ = ioInterp (cps (ast6, [], k))

  val ast7 =
    LIf ((LBool true), (LApp ((LVar "#output"), (LStr " tru branch !!!!"))), (LApp ((LVar "#output"), (LStr "fals branch !!!!"))))
  val _ = ioInterp (cps (ast7, [], k))


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
void(kInit! []);

(*   (* eq(2, 2, fn pred => *) *)
(*   (*   if pred then *) *)
(*   (*     println("atas", k) *) *)
(*   (*   else *) *)
(*   (*     println("bawah", k)); *) *)

(* fun seq(stmt1, stmt2, k) = *)
(*   stmt1(fn _ => *)
(*     stmt2(k)); *)
