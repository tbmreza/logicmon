use "../CPS/cps.sml";  (* ??: can't use use when sources.cm grows *)

structure CommandLine = struct
    datatype flag = 
        Help | Version | Verbose | Debug | Output of string | Input of string
    
    datatype config = Config of {
        help: bool,
        version: bool,
        verbose: bool,
        debug: bool,
        input_file: string option,
        output_file: string option,
        remaining_args: string list
    }
    
    (* Catch-all exception for usage errors *)
    exception ParseError of string
    
    val default_config = Config {
        help = false,
        version = false, 
        verbose = false,
        debug = false,
        input_file = NONE,
        output_file = NONE,
        remaining_args = []
    }
    
    fun update_config (Config {help, version, verbose, debug, input_file, output_file, remaining_args}) flag =
        case flag of
            Help => Config {help=true, version=version, verbose=verbose, debug=debug, 
                          input_file=input_file, output_file=output_file, remaining_args=remaining_args}
          | Version => Config {help=help, version=true, verbose=verbose, debug=debug,
                             input_file=input_file, output_file=output_file, remaining_args=remaining_args}
          | Verbose => Config {help=help, version=version, verbose=true, debug=debug,
                             input_file=input_file, output_file=output_file, remaining_args=remaining_args}
          | Debug => Config {help=help, version=version, verbose=verbose, debug=true,
                           input_file=input_file, output_file=output_file, remaining_args=remaining_args}
          | Input filename => Config {help=help, version=version, verbose=verbose, debug=debug,
                                    input_file=SOME filename, output_file=output_file, remaining_args=remaining_args}
          | Output filename => Config {help=help, version=version, verbose=verbose, debug=debug,
                                     input_file=input_file, output_file=SOME filename, remaining_args=remaining_args}
    
    fun parse_arg arg =
        case arg of
            "-h" => SOME Help
          | "--help" => SOME Help
          | "-V" => SOME Version
          | "--version" => SOME Version
          | "--verbose" => SOME Verbose
          | "-d" => SOME Debug
          | "--debug" => SOME Debug
          | _ => 
            if String.isPrefix "-o" arg then
                if String.size arg > 2 then
                    SOME (Output (String.extract(arg, 2, NONE)))
                else NONE (* -o requires argument *)
            else if String.isPrefix "--output=" arg then
                SOME (Output (String.extract(arg, 9, NONE)))
            else if String.isPrefix "-i" arg then
                if String.size arg > 2 then
                    SOME (Input (String.extract(arg, 2, NONE)))
                else NONE
            else if String.isPrefix "--input=" arg then
                SOME (Input (String.extract(arg, 8, NONE)))
            else NONE
    
    (* Parse arguments with state *)
    fun parse_args_helper ([], config, expecting_value, remaining) =
        (case expecting_value of
            NONE => (config, rev remaining)
          | SOME flag_name => raise ParseError ("Missing value for " ^ flag_name))
      | parse_args_helper (arg::rest, config, expecting_value, remaining) =
        (case expecting_value of
            SOME "output" => 
                parse_args_helper(rest, update_config config (Output arg), NONE, remaining)
          | SOME "input" =>
                parse_args_helper(rest, update_config config (Input arg), NONE, remaining)
          | SOME flag_name => raise ParseError ("Unknown flag expecting value: " ^ flag_name)
          | NONE =>
            (case parse_arg arg of
                SOME flag => parse_args_helper(rest, update_config config flag, NONE, remaining)
              | NONE =>
                if arg = "-o" then
                    parse_args_helper(rest, config, SOME "output", remaining)
                else if arg = "-i" then
                    parse_args_helper(rest, config, SOME "input", remaining)
                else if String.isPrefix "-" arg then
                    raise ParseError ("Unknown option: " ^ arg)
                else
                    parse_args_helper(rest, config, NONE, arg::remaining)))
    
    (* Main parsing function *)
    fun parse_args args =
        let
            val (Config {help, version, verbose, debug, input_file, output_file, remaining_args=_}, remaining) = 
                parse_args_helper(args, default_config, NONE, [])
        in
            Config {help=help, version=version, verbose=verbose, debug=debug,
                   input_file=input_file, output_file=output_file, remaining_args=remaining}
        end
    
    (* Pretty print configuration *)
    fun config_to_string (Config {help, version, verbose, debug, input_file, output_file, remaining_args}) =
        "Configuration:\n" ^
        "  Help: " ^ Bool.toString help ^ "\n" ^
        "  Version: " ^ Bool.toString version ^ "\n" ^
        "  Verbose: " ^ Bool.toString verbose ^ "\n" ^
        "  Debug: " ^ Bool.toString debug ^ "\n" ^
        "  Input file: " ^ (case input_file of NONE => "None" | SOME f => f) ^ "\n" ^
        "  Output file: " ^ (case output_file of NONE => "None" | SOME f => f) ^ "\n" ^
        "  Remaining args: [" ^ String.concatWith ", " remaining_args ^ "]"
    
    (* Help text ??: bin name, example usage *)
    val help_text = 
        "Usage: program [OPTIONS] [FILES...]\n\n" ^
        "Options:\n" ^
        "  -h, --help           Show this help message\n" ^
        "  -V, --version        Show version information\n" ^
        "      --verbose        Enable verbose output\n" ^
        "  -d, --debug         Enable debug mode\n" ^
        "  -i FILE             Input file\n" ^
        "      --input=FILE     Input file (alternative syntax)\n" ^
        "  -o FILE             Output file\n" ^
        "      --output=FILE    Output file (alternative syntax)\n\n" ^
        "Examples:\n" ^
        "  program -i input.txt -o output.txt\n" ^
        "  program --input=data.txt --verbose file1.txt file2.txt\n" ^
        "  program -d --output=result.txt *.sml"
    
    (* Interpret cli config. *)
    fun run_app config =
        let
            val Config {help, version, verbose, debug, input_file, output_file, remaining_args} = config
            val version_text = Interpreter.instanceInfo ^ "\n"
        in
            if help then
                print (help_text ^ "\n")
            else if version then
                print version_text
            else
                (if verbose then print "Running in verbose mode\n" else ();
                 if debug then print "Debug mode enabled\n" else ();
                 case input_file of
                    SOME f => print ("Processing input file: " ^ f ^ "\n")
                  | NONE => ();
                 case output_file of
                    SOME f => print ("Output will be written to: " ^ f ^ "\n")
                  | NONE => ();
                 if not (null remaining_args) then
                    print ("Additional files: " ^ String.concatWith " " remaining_args ^ "\n")
                 else ())
        end
end

(* Example usage and test cases *)
structure Main = struct
    open CommandLine
    
    (* Test function with concrete examples *)
    fun test_examples () =
        let
            val test_cases = [
                ["--help"],
                ["--verbose", "-d", "file1.txt", "file2.txt"],
                ["-i", "input.txt", "-o", "output.txt"],
                ["--input=data.txt", "--output=results.txt", "--verbose"],
                ["-ooutput.txt", "-iinput.txt", "--debug"],
                ["file1.txt", "file2.txt", "file3.txt"],
                ["-V"]
            ]
            
            fun run_test (i, args) =
                (print ("Test " ^ Int.toString i ^ ": " ^ String.concatWith " " args ^ "\n");
                 let
                     val config = parse_args args
                 in
                     print (config_to_string config ^ "\n");
                     run_app config;
                     print "---\n"
                 end
                 handle ParseError msg => print ("Parse Error: " ^ msg ^ "\n---\n"))
        in
            List.appi run_test test_cases
        end
end

(* In repl *)
(* Main.test_examples(); *)
