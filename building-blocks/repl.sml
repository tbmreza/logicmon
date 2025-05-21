(* Simple REPL (Read-Eval-Print Loop) for Standard ML using Poly/ML *)

exception REPLExit;

fun evalString str =
    let
        val lexbuf = Compiler.lex (Compiler.mkCompilationContext []) (Byte.stringToBytes str)
        val stream = Compiler.parseWithLexbuf lexbuf
        val result = Compiler.eval stream
    in
        result
    end
    handle e => "Error: " ^ (General.exnMessage e);

fun trim str =
    let
        fun trimLeft (#" "::cs) = trimLeft cs
          | trimLeft (#"\t"::cs) = trimLeft cs
          | trimLeft (#"\n"::cs) = trimLeft cs
          | trimLeft (#"\r"::cs) = trimLeft cs
          | trimLeft cs = cs

        fun trimRight [] = []
          | trimRight [c] = if c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r" then [] else [c]
          | trimRight (c::cs) = 
                let val rest = trimRight cs
                in if null rest andalso (c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r")
                   then [] else c::rest
                end
    in
        implode (trimRight (trimLeft (explode str)))
    end;

fun repl () =
    let
        val _ = print "Poly/ML REPL - Type 'exit;' to quit\n"
        
        fun loop () =
            let
                val _ = print "\n> "
                val input = TextIO.inputLine TextIO.stdIn
            in
                case input of
                    NONE => ()
                  | SOME line =>
                        let val trimmed = trim line
                        in
                            if trimmed = "exit;" then raise REPLExit
                            else if trimmed = "" then loop()
                            else (print (evalString line ^ "\n"); loop())
                        end
            end
    in
        (loop()) handle REPLExit => print "Exiting REPL...\n"
    end;

(* Run the REPL *)
val _ = repl();
