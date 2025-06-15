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

  (* ??: user provided config from cli *)
  (* fun tokensList(cliConf { sourcePath }) =  *)

  fun mtProgram() = [PosterLrVals.Tokens.EOF(0,0)]

  fun load sourcePath =
    let
      val stream = TextIO.openIn sourcePath
      val lexer =
        PosterParser.makeLexer(
          fn i => TextIO.inputN(stream,i))

      val tEOF = PosterLrVals.Tokens.EOF(0,0)
      (* val dummySEMI = PosterLrVals.Tokens.SEMI(0,0) *)

      fun loop lexer =
        let
          val (result,lexer) = invoke lexer
          val (nextToken,lexer) = PosterParser.Stream.get lexer
          val _ = case result of
                       SOME r => TextIO.output(TextIO.stdOut, "result = " ^ (Int.toString r) ^ "\n")
                       (* SOME (SOME r) => TextIO.output(TextIO.stdOut, "result = " ^ (Int.toString r) ^ "\n") *)
                     | NONE => ()
        in
          if PosterParser.sameToken(nextToken,tEOF) then () else loop lexer
        end
    in loop lexer
    end
end
