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


(* CPS conversion function F *)

datatype stmt =
  (* LExprStmt of expr *)
  (* LIf of expr * stmt * stmt *)
  LIf of expr * expr * expr
| LIo of expr   (* ??: probably poisoned *)
and expr =
  (* LLet of (string * expr) list * stmt  (* LLet { [(name, expr)], body } *) *)
  LLet of (string * expr) list * expr
| LInt of int
| LStr of string
| LBool of bool
| LApp of expr * expr
| LVar of string
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

fun lookup x [] = raise Fail ("Unbound variable: " ^ x)
  | lookup x ((y, v)::env) = if x = y then v else lookup x env;

fun io_interp (OUTPUT s) = (print(s ^ "\n"); UNIT)
  | io_interp (INT n) = (n; UNIT)
  | io_interp (REF v) =
    case v of
      INT n => (n; UNIT)
;
fun toString(STRING s) = s
  | toString(INT v) = Int.toString v
  | toString(BOOL v) = Bool.toString v
  | toString(REF v) = toString v

;


fun output_lookup(name, env, k) = k(lookup name env);

fun cps (LInt v, _, k) = k (INT v)
  | cps (LStr v, _, k) = k (STRING v)


  | cps (LVar name, env, k) = let
    val got = lookup name env
  in
    k (REF got)
  end

  | cps (LApp ((LVar "#output"), arg), env, k) =
    cps (arg, env, fn got => k (OUTPUT (toString got)))


  (*   output_lookup(name, env, fn v => ) *)

  (* name => { output name} *)

  (* | cps (LLet ([], body), env, k) = in_cps (body, k) *)
  (* | cps (LApp ((LVar "#output"), LInt v), _, k) =  k (OUTPUT (toString (LInt v))) *)
  (* | cps (LApp ((LVar "#output"), LStr v), _, k) =  k (OUTPUT (toString (LStr v))) *)
  (* | cps (LApp ((LVar "#output"), LBool v), _, k) =  k (OUTPUT (toString (LBool v))) *)
;

fun in_cps(LInt n, k) = k (INT n)
  | in_cps(LStr s, k) = k (STRING s)

  (* | in_cps(LVar name, env, k) = *)
  (*   (* let val got = lookup name env in *) *)
  (*   (* let val got = 8 in *) *)
  (*   k (INT 9) *)
  (*   (* end *) *)

  | in_cps(LLet ([], body), k) = in_cps (body, k)

  (* | in_cps(LLet ([(varname, expr)], body), k) = in_cps (body, k) *)

  | in_cps(LApp ((LVar "#output"), (LStr s)), k) =  k (OUTPUT s)
  | in_cps(LApp ((LVar "#output"), (LInt v)), k) =  k (OUTPUT (Int.toString v))
  | in_cps(LApp ((LVar "#output"), (LBool v)), k) = k (OUTPUT (Bool.toString v))

  (* | in_cps(LApp ((LVar "#output"), e), k) = in_cps (LApp ((LVar "#output"), in_cps (e, k))) *)

;


fun stmt1(k) = k(print("stmt1"));
fun stmt2(k) = k(print("stmtB"));

  (* ??: save_continuation before applying it *)
fun put(arg, k) =
  k(print(arg));

fun println(arg, k) =
  put(arg, fn _ => k(print("\n")));

fun eq(x, y, k) = k(x = y);

fun letin(str, k) = k(str);

fun void(k) =
  (* let val ast = LLet([("a1", LStr "bound")], LVar "a1") in *)
  (* let val ast = LLet([("a1", LStr "bound")], (LVar "a1")) in *)
  let
  (* val ast = LLet([("a1", LStr "bound")], LApp ((LVar "#output"), (LVar "a1")))  (* a1 := "bound"; #output a1 *) *)
  (* val asu = LIf ((LBool true), (LIo (LApp ((LVar "#output"), (LStr "atas")))), (LIo (LApp ((LVar "#output"), (LStr "bawah"))))) *)

  (* val ast3 = (LApp ((LVar "#output"), (LBool true)), k) *)
  (* val ast4 = (LLet([], LApp ((LVar "#output"), (LStr "from let block"))), k) *)
  (* val ast5 = LApp ((LVar "#output"), (LVar "status")) *)


  val ast1 = LApp ((LVar "#output"), (LStr "something good happened !!!!"))
  val do1 = io_interp (cps (ast1, [], k))

  val ast2 = LApp ((LVar "#output"), (LVar "pi"))
  val do2 = io_interp (cps (ast2, [("pi", (INT 314))], k))

  (* val ast3 = LApp ((LVar "#output"), (LVar "pi")) *)
  (* (* val do3 = io_interp (cps (ast3, [("pi", (INT 3))], k)) *) *)

  in
    (* in_cps(LLet ([], body), k); *)
    (* println("bisa", k); *)
    print("")
  end;
void(fn x => x);

fun TEST(k) =
  (* println("hore", (fn _ => println("berhasil", k))); *)
  (* letin("okwwdoki", fn a1 => println(a1, k)); *)
  in_cps(LInt 999, fn x => x);  (* ??: print string with if-stmt; make up user of store; "save_continuation" *)
  (* in_cps(LIf ((LBool true), (LIo (LApp ((LVar "#output"), (LStr "atas")))), (LIo (LApp ((LVar "#output"), (LStr "bawah"))))), fn x => x); *)

  (* eq(2, 2, fn pred => *)
  (*   if pred then *)
  (*     println("atas", k) *)
  (*   else *)
  (*     println("bawah", k)); *)
  ;

(* TEST(fn x => x); *)
TEST(fn x => UNIT);

(* back *)
(* function save_continuation(cont) { *)
(*     snapshot = Continuation { *)
(*         function: cont, *)
(*         args: current_args, *)
(*         environment: environment.clone(), *)
(*         call_stack: call_stack.clone(),  *)
(*         timestamp: now(), *)
(*         parent: debugger.current_continuation() *)
(*     } *)
(*      *)
(*     debugger.history.append(snapshot) *)
(*     debugger.current_index = debugger.history.length - 1 *)
(*      *)
(*     // Check for breakpoints or watches *)
(*     if should_break(snapshot) { *)
(*         enter_debug_mode(snapshot) *)
(*     } *)
(* } *)

fun seq(stmt1, stmt2, k) =
  stmt1(fn _ =>
    stmt2(k));

fun PROG(k) =
  (* seq(stmt1, fn k2 => k2(print("stmt2")), k); *)

  (* let flow = seq(stmt1, stmt2, k) in *)

  (* seq(stmt1, stmt2, k) end; *)
  14;

PROG(fn x => x);
