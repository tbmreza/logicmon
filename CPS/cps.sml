(*
StringCopy(s, n)
for i := 1 to n
	t[i] := s[i]
return t

#output StringCopy("Poster", 255)  -- outputs "Poster" 

a1 := "bound"
#output a1
#output 12
#output "ok"


Fn { name, args, body }


#output 12   Put { printable }
... transformed to ...  ??
cps_output 12 toString
toString : Print -> String

cps_output : Print -> (Print -> String) -> String


*)

fun stringCopy(s, n) =
    let
        val t = ref "";  (* Use a reference to simulate a mutable string *)

        fun copyHelper i =
            if i > n then ()
            else if i > String.size s then ()
            else (
                t := !t ^ str (String.sub(s, i - 1));
                copyHelper (i + 1)
            );

        (* Start the copying process *)
        val _ = copyHelper 1
    in
        !t  (* Dereference to get the final string *)
    end;

(* val result = stringCopy("Poster", 255); *)
(* print(result); *)

fn nil => 0
 | x::nil => 1
 | z => 2;


(* Abstract syntax



*)

datatype stmt =
  (* LExprStmt of expr *)
  (* LIf of expr * stmt * stmt *)
  LIf of   expr * expr * expr
| LIo of   expr
and expr =
  LLet of  (string * expr) * expr
| LInt of  int
| LMul of  expr list
| LStr of  string
| LBool of bool
| LApp of  expr * expr
| LVar of  string
(* | LIo of stmt *)
;

datatype value =
  INT of int
| STRING of string
| BOOL of bool
| OUTPUT of string
| REF of value
| UNIT
;
type env = (string * value) list;

(* It matters that env is an ordered list (of history of values)
   that `lookup` returns early on the closest/most local value
   of bound variable. *)
fun lookup x [] = raise Fail ("Unbound variable: " ^ x)
  | lookup x ((y, v)::env) = if x = y then v else lookup x env;

fun extend x v env = (x, v)::env;

(* Presumably with `io_interp : value -> UNIT` halt/non-halting
   k differentiation is rendered unnecessary. *)
fun io_interp (OUTPUT s) = (print(s ^ "\n"); UNIT)
  | io_interp (INT n) = (n; UNIT)
  | io_interp (REF v) =
    case v of
      INT n =>    (n; UNIT)
    | STRING s => (s; UNIT)
    | BOOL b =>   (b; UNIT)
    | UNIT =>     ((); UNIT)
;
fun toString(STRING s) = s
  | toString(INT v) = Int.toString v
  | toString(BOOL v) = Bool.toString v
  | toString(REF v) = toString v
;

(* ??: recursive value unwrap? *)
fun from_ref (REF (INT n)) = LInt n
  | from_ref (INT n) = LInt n
  (* | from_ref r = (print("from_ref:\t" ^ r ^ "\n"); LInt 0) ??: show value *)


(* Evaluate AST to a value via CPS rules. Saves env on every interesting
   reductions (cps invocations in most rhs').
    cps :  stmt/expr  -> value  *)
fun cps (LInt v, _, k) = k (INT v)

  | cps (LMul [(LInt m), (LInt n)], env, k) =
    k (INT (m * n))

  | cps (LMul [la, lb], env, k) =
    cps (la, env, fn la' =>
      cps (lb, env, fn lb' =>
        cps (LMul [(from_ref la'), (from_ref lb')], env, k)))


  | cps (LStr v, _, k) = k (STRING v)

  | cps (LVar name, env, k) = let
    val got = lookup name env
  in
    k (REF got)
  end

  | cps (LApp ((LVar "#output"), arg), env, k) =
    cps (arg, env, fn got =>
    (print("putting...\n");
    k (OUTPUT (toString got))))
    (* _save
    (k (OUTPUT (toString got)), env) *)

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
    cps (exp, env, fn exp' => cps (body, (extend name exp' env), k))


fun stmt1(k) = k(print("stmt1"));
fun stmt2(k) = k(print("stmtB"));

  (* ??: save_continuation before applying it *)
fun put(arg, k) =
  k(print(arg));

fun println(arg, k) =
  put(arg, fn _ => k(print("\n")));

fun eq(x, y, k) = k(x = y);

fun letin(str, k) = k(str);

val SNAPSHOT : (env option) ref = ref NONE

(* fn _save(def, e) = *)
(*   let *)
(*     val _ = SNAPSHOT := SOME e *)
(*   in *)
(*     def *)
(*   end *)
(* _save(fn x => x, []) *)

fun init_k(e: env) =
  let
    val _ = SNAPSHOT := SOME e
  in
    fn x => x
  end

fun void(k) =
  let
  (* val asu = LIf ((LBool true), (LIo (LApp ((LVar "#output"), (LStr "atas")))), (LIo (LApp ((LVar "#output"), (LStr "bawah"))))) *)

  (* ??: Poster.parsePath "examples/Output.al" < '#output "something"' *)
  (*  "concrete interpreter"
  CPS/cps.sml requires grammar/parser.sml for parsePath : Path -> ast
  CPS/cps.sml provides                        io_interp : value -> IO ()
  cli.sml requires io_interp, for example `io_interp (cps "#output 12")`



  "time travel"

  type state = cont
  , trace : , cps :  -> [state]
  env_from_cont : fn -> dict<name, expr/value>
  rewind : _::[state] -> [state]
  goto : [state] -> index -> state

  *)
  val ast1 = LApp ((LVar "#output"), (LStr "something good happened !!!!"))
  val do1 = io_interp (cps (ast1, [], k))

  val ast3 = LApp ((LVar "#output"), LMul [LInt 2, LInt 9])
  val do3 = io_interp (cps (ast3, [("Kilo", (INT 1000))], k))

  val ast2 = LApp ((LVar "#output"), (LVar "pi"))
  val do2 = io_interp (cps (ast2, [("pi", (INT 415))], k))

  val ast = LApp ((LVar "#output"), LMul [LInt 2, (LVar "Kilo")])
  val _ = io_interp (cps (ast, [("Kilo", (INT 1000))], k))

  val ast4 =
    LLet (("user", LStr "09"),
      LApp ((LVar "#output"), (LVar "user")))
  val do4 = io_interp (cps (ast4, [], k))

  val ast5 =
    LLet (("user", LStr "08"), LLet (("user", LStr "07"), LApp ((LVar "#output"), (LVar "user"))))
  val do5 = io_interp (cps (ast5, [], k))

  val ast6 =
    LLet (("user", LStr "3..."),
      LLet (("user", LStr "2.."),
        LLet (("user", LStr "1."), LApp ((LVar "#output"), (LVar "user")))))
  val do6 = io_interp (cps (ast6, [], k))
    

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
void(init_k []);

(*   (* eq(2, 2, fn pred => *) *)
(*   (*   if pred then *) *)
(*   (*     println("atas", k) *) *)
(*   (*   else *) *)
(*   (*     println("bawah", k)); *) *)

(* fun seq(stmt1, stmt2, k) = *)
(*   stmt1(fn _ => *)
(*     stmt2(k)); *)
