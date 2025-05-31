(* structure Poster : sig
	           val parse : unit -> unit
                 end =  *)
structure Poster =

struct
    structure PosterLrVals = PosterLrValsFun(structure Token = LrParser.Token)
    structure PosterLex =    PosterLexFun(structure Tokens = PosterLrVals.Tokens)
    structure PosterParser = Join(
        structure LrParser = LrParser
        structure ParserData = PosterLrVals.ParserData
        structure Lex = PosterLex)

    fun invoke lexstream =
        let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
        in PosterParser.parse(0,lexstream,print_error,())
        end

    fun parsePath s =
        let
            val dev = TextIO.openIn s
            val lexer = PosterParser.makeLexer(fn i => TextIO.inputN(dev,i))
            val dummyEOF = PosterLrVals.Tokens.EOF(0,0)
            val dummySEMI = PosterLrVals.Tokens.SEMI(0,0)

            fun loop lexer =
                let
                    val (result,lexer) = invoke lexer
                    val (nextToken,lexer) = PosterParser.Stream.get lexer
                    val _ = case result of
                                 SOME r => TextIO.output(TextIO.stdOut, "result = " ^ (Int.toString r) ^ "\n")
                               | NONE => ()
                in
                    if PosterParser.sameToken(nextToken,dummyEOF) then () else loop lexer
                end

        (* in 0 *)
        in loop lexer
        end
end