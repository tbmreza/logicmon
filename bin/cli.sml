(* poster --version *)
(* poster string-copy.al *)


datatype ArgType =                 (* Example: *)
    Flag of       bool ref         (* -v or --verbose *)
  | StringArg of  string ref       (* -f file.txt *)
  | IntArg of     int ref          (* -n 10 *)
  | StringList of string list ref  (* -I dir1 -I dir2 *)

type ArgDescriptor = {
    shortName: string option,      (* -v *)
    longName: string option,       (* --verbose *)
    description: string,           (* Description for help text *)
    argType: ArgType               (* Type of argument *)
};

exception ArgParseError of string;

(* Struct with mutable fields. *)
structure ArgParser = struct
    val arguments: ArgDescriptor list ref = ref []
    val showHelp = ref true
    
    fun registerArg (arg: ArgDescriptor) =
        arguments := arg :: (!arguments)

    fun findArg [] _ _ = NONE
      | findArg (arg::args) shortName longName =
        case arg of
            (* {shortName=SOME s, longName=_, description=_, argType=_} =>  ?? *)
            (*     if s = shortName then SOME arg else findArg args shortName longName *)
            {shortName=NONE, longName=SOME l, description=_, argType=_} => 
                if l = longName then SOME arg else findArg args shortName longName
          | {shortName=SOME s, longName=SOME l, description=_, argType=_} => 
                if s = shortName orelse l = longName then SOME arg else findArg args shortName longName
          | _ => findArg args shortName longName
    

    fun processArg (argName, valueOpt, remainingArgs) =
        let
            val (isShort, name) = 
                if String.isPrefix "--" argName then
                    (false, String.extract(argName, 2, NONE))
                else if String.isPrefix "-" argName then
                    (true, String.extract(argName, 1, NONE))
                else
                    raise ArgParseError ("Unknown argument format: " ^ argName)
                    
            val arg = findArg (!arguments) (if isShort then name else "") (if isShort then "" else name)
        in
            case arg of
                NONE => raise ArgParseError ("Unknown argument: " ^ argName)
              | SOME {argType=Flag ref_val, ...} => 
                    (ref_val := true; remainingArgs)
              | SOME {argType=StringArg ref_val, ...} => 
                    (case valueOpt of
                         NONE => 
                             (case remainingArgs of
                                  [] => raise ArgParseError ("Missing value for " ^ argName)
                                | value::rest => (ref_val := value; rest))
                       | SOME value => (ref_val := value; remainingArgs))
              | SOME {argType=IntArg ref_val, ...} => 
                    (case valueOpt of
                         NONE => 
                             (case remainingArgs of
                                  [] => raise ArgParseError ("Missing value for " ^ argName)
                                | value::rest => 
                                    ((ref_val := valOf (Int.fromString value)
                                      handle Option => raise ArgParseError ("Invalid integer value for " ^ argName));
                                     rest))
                       | SOME value => 
                           ((ref_val := valOf (Int.fromString value)
                             handle Option => raise ArgParseError ("Invalid integer value for " ^ argName));
                            remainingArgs))
              | SOME {argType=StringList ref_val, ...} => 
                    (case valueOpt of
                         NONE => 
                             (case remainingArgs of
                                  [] => raise ArgParseError ("Missing value for " ^ argName)
                                | value::rest => (ref_val := value :: (!ref_val); rest))
                       | SOME value => (ref_val := value :: (!ref_val); remainingArgs))
        end

    fun processArgs [] = []
      | processArgs (arg::args) =
        if String.isPrefix "-" arg then
            let
                (* Check for argument with attached value like -f=file.txt *)
                val parts = String.fields (fn c => c = #"=") arg
            in
                case parts of
                    [name, value] => processArgs (processArg (name, SOME value, args))
                  | [name] => processArgs (processArg (name, NONE, args))
                  | _ => raise ArgParseError ("Invalid argument format: " ^ arg)
            end
        else
            arg :: processArgs args

    fun parse () =
        (* Built-in subcommand flags *)
        let
            val _ = registerArg {
                shortName = SOME "h",
                longName = SOME "help",
                description = "Show this help message",
                argType = Flag showHelp
            }

            val _ = registerArg {
                shortName = SOME "V",
                longName = SOME "version",
                description = "Print version info and exit",
                argType = Flag showHelp
            }

            val cmdLineArgs = CommandLine.arguments()
            val remainingArgs = processArgs cmdLineArgs
        in
            (* If help flag is set, show help *)
            if !showHelp then (printHelp(); remainingArgs) else remainingArgs
        end

    (* Generate and print help text *)
    and printHelp () =
        let
            (* fun argToString {shortName, longName, description, ...} = *)
            fun argToString {shortName, longName, description, argType as _} =
                let
                    val shortStr = case shortName of SOME s => "-" ^ s | NONE => ""
                    val longStr = case longName of SOME l => "--" ^ l | NONE => ""
                    val nameStr = 
                        if shortStr <> "" andalso longStr <> "" then
                            shortStr ^ ", " ^ longStr
                        else
                            shortStr ^ longStr
                in
                    "  " ^ nameStr ^ "\t" ^ description
                end
            
            val helpLines = map argToString (!arguments)
            val programName = CommandLine.name()
        in
            print ("Usage: " ^ programName ^ " [options] [arguments]\n\n");
            print "Options:\n";
            app (fn line => print (line ^ "\n")) helpLines;
            print "\n"
        end
end;

(* Example usage *)
fun main () =
    let
        val verbose = ref false
        val printsVersion = ref true
        val filename = ref ""
        val count = ref 0
        val threshold = ref 0.0
        val includeDirs = ref ([] : string list)

        (* Register arguments *)

        (* val _ = ArgParser.registerArg { *)
        (*     shortName = SOME "V", *)
        (*     longName = SOME "version", *)
        (*     description = "Print version info and exit", *)
        (*     argType = Flag printsVersion *)
        (* } *)

        (* val _ = ArgParser.registerArg { *)
        (*     shortName = SOME "f", *)
        (*     longName = SOME "file", *)
        (*     description = "Input file", *)
        (*     argType = StringArg filename *)
        (* } *)
        (*  *)
        (* val _ = ArgParser.registerArg { *)
        (*     shortName = SOME "n", *)
        (*     longName = SOME "count", *)
        (*     description = "Number of iterations", *)
        (*     argType = IntArg count *)
        (* } *)
        (*  *)
        (* val _ = ArgParser.registerArg { *)
        (*     shortName = SOME "I", *)
        (*     longName = NONE, *)
        (*     description = "Include directory (can be repeated)", *)
        (*     argType = StringList includeDirs *)
        (* } *)
        (*  *)

        val remainingArgs = ArgParser.parse()

        fun boolToString b = if b then "true" else "false"

        (* fun printStringList [] = print "none\n" *)
        (*   | printStringList [x] = print (x ^ "\n") *)
        (*   | printStringList (x::xs) = (print (x ^ ", "); printStringList xs) *)
        (* val {cmdVersion} = {filename = ArgParser.filename, verbose = ArgParser.verbose} *)
    in
        print "Parsed arguments:\n"
        (* ; *)
        (* print ("  verbose: " ^ boolToString (!verbose) ^ "\n"); *)
        (* print ("  filename: " ^ (!filename) ^ "\n"); *)
        (* print ("  count: " ^ Int.toString (!count) ^ "\n"); *)
        (* print ("  threshold: " ^ Real.toString (!threshold) ^ "\n"); *)
        (* print ("  include dirs: "); *)
        (* printStringList (rev (!includeDirs)); *)
        (* print "\nRemaining arguments: "; *)
        (* printStringList remainingArgs *)
    end;
val _ = main();
