structure Opt =
(* Do AST-level optimizations here. *)
(* ??: ast type checking with gadt or phantom types in SML so we can
   do...

   LIf of bool expr * 'a expr * 'a expr

   ...statically here. *)
struct
  (* ??: open Types *)
  val opt : unit -> (Poster.PosterLrVals.Tokens.svalue,int) Poster.PosterLrVals.Tokens.token list =
  fn () => Poster.mtProgram()

  fun run() = Poster.load "../examples/draft.al"
end
val _ = Opt.run();
(* cli -> parser -> opt -> interpreter *)
(*                 expr -> expr -> value *)
