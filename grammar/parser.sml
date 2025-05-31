structure Poster : sig
	           val parse : unit -> unit
                 end = 
struct

  structure PosterLrVals =
    PosterLrValsFun(structure Token = LrParser.Token)

  structure PosterLex =
    PosterLexFun(structure Tokens = PosterLrVals.Tokens)

  structure PosterParser =
    Join(structure LrParser = LrParser
	 structure ParserData = PosterLrVals.ParserData
	 structure Lex = PosterLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in PosterParser.parse(0,lexstream,print_error,())
      end


  fun parse () = 
      let val lexer = PosterParser.makeLexer (fn _ =>
                                               (case TextIO.inputLine TextIO.stdIn
                                                of SOME s => s
                                                 | _ => ""))
	  val dummyEOF = PosterLrVals.Tokens.EOF(0,0)
	  val dummySEMI = PosterLrVals.Tokens.SEMI(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = PosterParser.Stream.get lexer
		  val _ = case result
			    of SOME r =>
				TextIO.output(TextIO.stdOut,
				       "result = " ^ (Int.toString r) ^ "\n")
			     | NONE => ()
	       in if PosterParser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
       in loop lexer
      end

end