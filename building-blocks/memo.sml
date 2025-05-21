(* Memoization in Standard ML (Poly/ML) *)

(* Generic memoization structure with various implementations *)
structure Memo = struct
    (* Simple memoization using association list for functions with one argument *)
    fun memoize1 (f: 'a -> 'b) : 'a -> 'b =
        let
            (* Cache as association list *)
            val cache = ref ([] : ('a * 'b) list)
            
            (* Lookup function *)
            fun lookup [] _ = NONE
              | lookup ((k, v)::rest) key = 
                if k = key then SOME v else lookup rest key
                
            (* Memoized function *)
            fun memoized x =
                case lookup (!cache) x of
                    SOME result => result
                  | NONE => 
                        let
                            val result = f x
                            val _ = cache := (x, result) :: (!cache)
                        in
                            result
                        end
        in
            memoized
        end
        
    (* Memoization using association list for functions with two arguments *)
    fun memoize2 (f: 'a * 'b -> 'c) : 'a * 'b -> 'c =
        let
            (* Cache as association list *)
            val cache = ref ([] : (('a * 'b) * 'c) list)
            
            (* Custom equality check for pairs *)
            fun pairEq ((a1, b1), (a2, b2)) = a1 = a2 andalso b1 = b2
            
            (* Lookup function *)
            fun lookup [] _ = NONE
              | lookup ((k, v)::rest) key = 
                if pairEq (k, key) then SOME v else lookup rest key
                
            (* Memoized function *)
            fun memoized (x, y) =
                case lookup (!cache) (x, y) of
                    SOME result => result
                  | NONE => 
                        let
                            val result = f (x, y)
                            val _ = cache := ((x, y), result) :: (!cache)
                        in
                            result
                        end
        in
            memoized
        end

    (* Memoization with hash tables for better performance *)
    structure HashMemo = struct
        (* Polymorphic hash table implementation *)
        functor HashTableFn (structure Hash: sig
                                val hash: 'key -> word
                                val equal: 'key * 'key -> bool
                            end) = struct
            type 'a table = (Hash.key * 'a) list array * int ref
            
            fun new size = (Array.array(size, []), ref 0)
            
            fun insert (arr, count) key value =
                let
                    val idx = Word.toInt(Hash.hash key) mod (Array.length arr)
                    val bucket = Array.sub(arr, idx)
                    
                    (* Remove any existing entries with the same key *)
                    val newBucket = (key, value) :: 
                                    List.filter (fn (k, _) => not (Hash.equal(key, k))) bucket
                                    
                    val _ = Array.update(arr, idx, newBucket)
                    val _ = if List.length newBucket > List.length bucket 
                            then count := !count + 1 
                            else ()
                in
                    ()
                end
                
            fun lookup (arr, _) key =
                let
                    val idx = Word.toInt(Hash.hash key) mod (Array.length arr)
                    val bucket = Array.sub(arr, idx)
                    
                    fun find [] = NONE
                      | find ((k, v)::rest) = 
                            if Hash.equal(key, k) then SOME v else find rest
                in
                    find bucket
                end
        end
        
        (* Hash implementation for integers *)
        structure IntHash = struct
            fun hash (i: int) = Word.fromInt i
            fun equal (i1: int, i2: int) = i1 = i2
        end
        
        (* Hash implementation for strings *)
        structure StringHash = struct
            fun hash (s: string) = 
                let
                    val len = String.size s
                    
                    fun loop i h =
                        if i >= len then h
                        else 
                            let
                                val c = Char.ord(String.sub(s, i))
                                val h' = Word.+(Word.*(h, 0w65599), Word.fromInt c)
                            in
                                loop (i+1) h'
                            end
                in
                    loop 0 0w0
                end
                
            fun equal (s1: string, s2: string) = s1 = s2
        end
        
        (* Hash implementation for pairs *)
        functor PairHashFn (structure HashA: sig
                                  val hash: 'a -> word
                                  val equal: 'a * 'a -> bool
                              end
                            structure HashB: sig
                                  val hash: 'b -> word
                                  val equal: 'b * 'b -> bool
                              end) = struct
            fun hash (a: 'a, b: 'b) = 
                Word.xorb(HashA.hash a, HashB.hash b)
                
            fun equal ((a1, b1): 'a * 'b, (a2, b2): 'a * 'b) = 
                HashA.equal(a1, a2) andalso HashB.equal(b1, b2)
        end
        
        (* Create specific hash table modules *)
        structure IntTable = HashTableFn(structure Hash = IntHash)
        structure StringTable = HashTableFn(structure Hash = StringHash)
        structure IntPairTable = HashTableFn(
            structure Hash = PairHashFn(
                structure HashA = IntHash
                structure HashB = IntHash
            )
        )
        
        (* Memoize function taking a single int argument *)
        fun memoizeInt (f: int -> 'a) : int -> 'a =
            let
                val table = IntTable.new 101  (* Prime number size *)
                
                fun memoized x =
                    case IntTable.lookup table x of
                        SOME result => result
                      | NONE => 
                            let
                                val result = f x
                                val _ = IntTable.insert table x result
                            in
                                result
                            end
            in
                memoized
            end
            
        (* Memoize function taking a single string argument *)
        fun memoizeString (f: string -> 'a) : string -> 'a =
            let
                val table = StringTable.new 101
                
                fun memoized x =
                    case StringTable.lookup table x of
                        SOME result => result
                      | NONE => 
                            let
                                val result = f x
                                val _ = StringTable.insert table x result
                            in
                                result
                            end
            in
                memoized
            end
            
        (* Memoize function taking two int arguments *)
        fun memoizeIntPair (f: int * int -> 'a) : int * int -> 'a =
            let
                val table = IntPairTable.new 101
                
                fun memoized args =
                    case IntPairTable.lookup table args of
                        SOME result => result
                      | NONE => 
                            let
                                val result = f args
                                val _ = IntPairTable.insert table args result
                            in
                                result
                            end
            in
                memoized
            end
    end
end;

(* Examples and usage demonstrations *)

(* Example 1: Factorial with simple memoization *)
fun factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1);

val memoFactorial = Memo.memoize1 factorial;

(* Example 2: Fibonacci with memoization *)
fun slowFibonacci n =
    if n <= 1 then n
    else slowFibonacci (n - 1) + slowFibonacci (n - 2);

(* Redefine using memoization *)
local
    fun fibHelper f n =
        if n <= 1 then n
        else f (n - 1) + f (n - 2)
in
    val fastFibonacci = let val memo = ref (fn n => 0)
                        in
                            memo := Memo.memoize1 (fibHelper (!memo));
                            !memo
                        end
end;

(* Example 3: Binomial coefficient with memoization *)
fun binomial (n, k) =
    if k = 0 orelse k = n then 1
    else binomial (n-1, k-1) + binomial (n-1, k);

val memoBinomial = Memo.memoize2 binomial;

(* Example 4: Hash-based memoization for better performance *)
val hashMemoFib = Memo.HashMemo.memoizeInt (fn n =>
    if n <= 1 then n
    else hashMemoFib (n - 1) + hashMemoFib (n - 2)
);

(* Timing function *)
fun time f x =
    let
        val timer = Timer.startRealTimer()
        val result = f x
        val elapsed = Timer.checkRealTimer timer
    in
        (result, Time.toReal elapsed)
    end;

(* Test and compare performance *)
fun runTests () =
    let
        (* Test factorial *)
        val _ = print "Testing factorial...\n"
        val n = 20
        val (result1, time1) = time factorial n
        val (result2, time2) = time memoFactorial n
        val _ = print ("  Regular factorial(" ^ Int.toString n ^ ") = " ^ 
                       Int.toString result1 ^ " (time: " ^ Real.toString time1 ^ "s)\n")
        val _ = print ("  Memoized factorial(" ^ Int.toString n ^ ") = " ^ 
                       Int.toString result2 ^ " (time: " ^ Real.toString time2 ^ "s)\n")
        
        (* Test fibonacci *)
        val _ = print "\nTesting fibonacci...\n"
        val n = 30
        val (result3, time3) = time slowFibonacci 20  (* Smaller n for slow version *)
        val (result4, time4) = time fastFibonacci n
        val (result5, time5) = time hashMemoFib n
        val _ = print ("  Regular fibonacci(20) = " ^ 
                       Int.toString result3 ^ " (time: " ^ Real.toString time3 ^ "s)\n")
        val _ = print ("  Memoized fibonacci(" ^ Int.toString n ^ ") = " ^ 
                       Int.toString result4 ^ " (time: " ^ Real.toString time4 ^ "s)\n")
        val _ = print ("  Hash memoized fibonacci(" ^ Int.toString n ^ ") = " ^ 
                       Int.toString result5 ^ " (time: " ^ Real.toString time5 ^ "s)\n")
        
        (* Test repeated calls with memoization *)
        val _ = print "\nTesting repeated calls...\n"
        val _ = time hashMemoFib n  (* Prime the cache *)
        val (_, time6) = time hashMemoFib n  (* Second call should be faster *)
        val _ = print ("  Repeated hash memoized fibonacci(" ^ Int.toString n ^ ") time: " ^ 
                       Real.toString time6 ^ "s\n")
        
        (* Test binomial coefficient *)
        val _ = print "\nTesting binomial coefficient...\n"
        val (n, k) = (25, 12)
        val (result7, time7) = time binomial (n, k)
        val (result8, time8) = time memoBinomial (n, k)
        val _ = print ("  Regular binomial(" ^ Int.toString n ^ "," ^ Int.toString k ^ ") = " ^ 
                       Int.toString result7 ^ " (time: " ^ Real.toString time7 ^ "s)\n")
        val _ = print ("  Memoized binomial(" ^ Int.toString n ^ "," ^ Int.toString k ^ ") = " ^ 
                       Int.toString result8 ^ " (time: " ^ Real.toString time8 ^ "s)\n")
    in
        ()
    end;

(* Run tests *)
val _ = runTests();
