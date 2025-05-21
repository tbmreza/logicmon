(* Stack and Heap Manipulation in Standard ML (Poly/ML) *)

(* 
 * Note: Unlike imperative languages, Standard ML doesn't provide direct control
 * over stack and heap allocations. Memory management is handled by the runtime
 * with garbage collection. However, we can demonstrate concepts that relate to
 * stack and heap usage and show how to manage memory efficiently.
 *)

(* ===== Stack Implementation ===== *)
structure Stack = struct
    (* Abstract type for stack *)
    type 'a stack = 'a list ref
    
    (* Create a new empty stack *)
    fun create () : 'a stack = ref []
    
    (* Check if stack is empty *)
    fun isEmpty (s: 'a stack) : bool = null (!s)
    
    (* Push item onto stack *)
    fun push (s: 'a stack) (x: 'a) : unit = s := x :: (!s)
    
    (* Pop item from stack *)
    fun pop (s: 'a stack) : 'a option =
        case !s of
            [] => NONE
          | x::xs => (s := xs; SOME x)
    
    (* Peek at top item without removing *)
    fun peek (s: 'a stack) : 'a option =
        case !s of
            [] => NONE
          | x::_ => SOME x
    
    (* Get stack size *)
    fun size (s: 'a stack) : int = length (!s)
    
    (* Clear the stack *)
    fun clear (s: 'a stack) : unit = s := []
    
    (* Convert stack to list (for inspection) *)
    fun toList (s: 'a stack) : 'a list = !s
end;

(* ===== Simple Heap Implementation ===== *)
structure Heap = struct
    (* 
     * We'll implement a binary min-heap using an array-based representation 
     * This is a heap data structure, not the ML memory heap
     *)
    
    (* Type for heap with comparison function *)
    type 'a heap = {
        data: 'a array ref,         (* Array to store elements *)
        size: int ref,              (* Current number of elements in heap *)
        capacity: int ref,          (* Current capacity of array *)
        lessThan: 'a * 'a -> bool   (* Comparison function *)
    }
    
    (* Helper functions for array index calculations *)
    fun parent i = (i - 1) div 2
    fun leftChild i = 2 * i + 1
    fun rightChild i = 2 * i + 2
    
    (* Swap elements at two indices *)
    fun swap {data, size, capacity, lessThan} i j =
        let
            val arr = !data
            val temp = Array.sub(arr, i)
        in
            Array.update(arr, i, Array.sub(arr, j));
            Array.update(arr, j, temp)
        end
    
    (* Heapify - maintain heap property *)
    fun heapify heap i =
        let
            val {data, size, capacity, lessThan} = heap
            val arr = !data
            val n = !size
            val left = leftChild i
            val right = rightChild i
            
            (* Find smallest among root, left child and right child *)
            val smallest = 
                if left < n andalso lessThan(Array.sub(arr, left), Array.sub(arr, i))
                then left
                else i
                
            val smallest' = 
                if right < n andalso lessThan(Array.sub(arr, right), Array.sub(arr, smallest))
                then right
                else smallest
        in
            (* If smallest is not root *)
            if smallest' <> i then (
                swap heap i smallest';
                heapify heap smallest'
            ) else ()
        end
    
    (* Create a new empty heap *)
    fun create (lessThan: 'a * 'a -> bool) (initialCapacity: int) : 'a heap =
        {
            data = ref (Array.array(initialCapacity, Option.valOf(Option.NONE))),  (* Dummy initialization *)
            size = ref 0,
            capacity = ref initialCapacity,
            lessThan = lessThan
        }
    
    (* Check if heap needs to expand *)
    fun checkAndExpand (heap as {data, size, capacity, lessThan}) =
        if !size >= !capacity then
            let
                val newCapacity = !capacity * 2
                val oldArr = !data
                val newArr = Array.array(newCapacity, Array.sub(oldArr, 0))  (* Dummy initialization *)
            in
                (* Copy elements to new array *)
                Array.copy{src = oldArr, dst = newArr, di = 0};
                data := newArr;
                capacity := newCapacity
            end
        else ()
    
    (* Insert a value into the heap *)
    fun insert (heap as {data, size, capacity, lessThan}) (x: 'a) =
        let
            (* Expand if needed *)
            val _ = checkAndExpand heap
            
            (* Insert at the end and increase size *)
            val arr = !data
            val i = !size
            val _ = Array.update(arr, i, x)
            val _ = size := i + 1
            
            (* Fix the min heap property if violated *)
            fun siftUp i =
                if i > 0 andalso lessThan(Array.sub(arr, i), Array.sub(arr, parent i)) then (
                    swap heap i (parent i);
                    siftUp (parent i)
                ) else ()
        in
            siftUp i
        end
    
    (* Extract the minimum element *)
    fun extractMin (heap as {data, size, capacity, lessThan}) : 'a option =
        case !size of
            0 => NONE
          | n => 
                let
                    val arr = !data
                    val minVal = Array.sub(arr, 0)
                    
                    (* Put the last element at root and reduce size *)
                    val _ = Array.update(arr, 0, Array.sub(arr, n-1))
                    val _ = size := n - 1
                    
                    (* Restore the heap property *)
                    val _ = if !size > 0 then heapify heap 0 else ()
                in
                    SOME minVal
                end
    
    (* Peek at the minimum without removing *)
    fun peek {data, size, capacity, lessThan} : 'a option =
        if !size = 0 then NONE
        else SOME (Array.sub(!data, 0))
    
    (* Get heap size *)
    fun size {data, size=s, capacity, lessThan} : int = !s
    
    (* Check if heap is empty *)
    fun isEmpty heap = size heap = 0
    
    (* Clear the heap *)
    fun clear (heap as {data, size, capacity, lessThan}) =
        size := 0
    
    (* Convert heap to a sorted list (destructive) *)
    fun toSortedList (heap as {data, size=s, capacity, lessThan}) =
        let
            val originalSize = !s
            fun extract acc =
                case extractMin heap of
                    NONE => acc
                  | SOME x => extract (x::acc)
        in
            extract [] |> List.rev
        end
end;

(* ===== Memory Analysis and Optimization Examples ===== *)

(* Example 1: Tail Recursion for Stack Efficiency *)

(* Non-tail-recursive factorial (can cause stack overflow for large inputs) *)
fun factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1);

(* Tail-recursive factorial (stack friendly) *)
fun factorialTail n =
    let
        fun loop 0 acc = acc
          | loop n acc = loop (n - 1) (n * acc)
    in
        loop n 1
    end;

(* Example 2: Avoiding Excessive Cons Operations *)

(* Inefficient list building (each :: creates garbage) *)
fun buildListInefficient n =
    if n <= 0 then []
    else n :: buildListInefficient (n - 1);

(* More efficient with tail recursion and accumulator *)
fun buildListEfficient n =
    let
        fun loop 0 acc = acc
          | loop i acc = loop (i - 1) (i :: acc)
    in
        loop n []
    end;

(* Example 3: Lazy Evaluation to Delay Heap Allocation *)

(* Thunk type for lazy evaluation *)
datatype 'a thunk = Thunk of (unit -> 'a);

(* Force evaluation of a thunk *)
fun force (Thunk f) = f ();

(* Lazy infinite list using thunks *)
datatype 'a lazylist = Nil | Cons of 'a * ('a lazylist thunk);

(* Generate an infinite list of integers from n *)
fun countFrom n = Cons(n, Thunk(fn () => countFrom (n + 1)));

(* Take first n elements from a lazy list *)
fun take 0 _ = []
  | take _ Nil = []
  | take n (Cons(x, xs)) = x :: take (n - 1) (force xs);

(* Example 4: Avoiding String Concatenation *)

(* Inefficient string building *)
fun concatStringsInefficient [] = ""
  | concatStringsInefficient (s::ss) = s ^ concatStringsInefficient ss;

(* More efficient using string buffer *)
fun concatStringsEfficient strings =
    let
        (* Calculate total length *)
        val totalLen = List.foldl (fn (s, acc) => acc + String.size s) 0 strings
        
        (* Create buffer *)
        val buffer = CharArray.array(totalLen, #" ")
        
        (* Fill buffer *)
        fun fill _ [] = ()
          | fill pos (s::ss) =
            let
                val len = String.size s
                val _ = CharArray.copyVec{src = s, dst = buffer, di = pos}
            in
                fill (pos + len) ss
            end
            
        val _ = fill 0 strings
    in
        CharArray.vector buffer
    end;

(* Example 5: Reference Cells and Mutation *)

(* Create a counter with reference cell (stored on heap) *)
fun makeCounter () =
    let
        val count = ref 0
        
        fun increment () = (count := !count + 1; !count)
        fun decrement () = (count := !count - 1; !count)
        fun reset () = (count := 0; !count)
        fun current () = !count
    in
        {increment = increment, decrement = decrement, 
         reset = reset, current = current}
    end;

(* Example 6: Stack Overflow Prevention with Continuation-Passing Style *)

(* Regular recursive function (stack-heavy) *)
fun deepRecursion 0 = 0
  | deepRecursion n = 1 + deepRecursion (n - 1);

(* CPS version to avoid stack overflows *)
fun deepRecursionCPS n =
    let
        fun loop 0 k = k 0
          | loop n k = loop (n - 1) (fn res => k (1 + res))
    in
        loop n (fn x => x)
    end;

(* ===== Demonstration of Memory Patterns ===== *)

(* Function to generate large temporary structures to observe GC behavior *)
fun generateGarbage size =
    let
        fun makeList 0 = []
          | makeList n = n :: makeList (n-1)
          
        val bigList = makeList size
        val _ = List.app (fn _ => ()) bigList  (* Force evaluation *)
    in
        "Generated and discarded a list of size " ^ Int.toString size
    end;

(* Function to demonstrate stack frames *)
fun recursiveStackDemo 0 = "Done"
  | recursiveStackDemo n =
    let
        (* Create some local bindings to make the stack frame larger *)
        val a = n * n
        val b = Real.fromInt n
        val c = Int.toString n
    in
        (* Recurse, creating a new stack frame *)
        recursiveStackDemo (n - 1)
    end;

(* Function to show heap fragmentation with many small allocations *)
fun fragmentHeap iterations =
    let
        (* Keep a reference to each allocation to prevent GC *)
        val refs = Array.array(iterations, ref 0)
        
        fun allocate 0 = ()
          | allocate i = 
            let
                (* Allocate a small object *)
                val r = ref i
                (* Store reference *)
                val _ = Array.update(refs, i-1, r)
            in
                allocate (i-1)
            end
    in
        allocate iterations;
        "Created " ^ Int.toString iterations ^ " small heap objects"
    end;

(* ===== Memory and GC Information Functions ===== *)

(* Get current memory usage statistics *)
fun getMemoryStats () =
    let
        val stats = Gc.statistics()
        
        (* Extract some interesting values *)
        val heapSize = #heapSize stats
        val allocations = #allocations stats
        val collections = #collections stats
    in
        "Memory Stats:\n" ^
        "  Heap Size: " ^ Word.toString heapSize ^ " words\n" ^
        "  Allocations: " ^ Word.toString allocations ^ " words\n" ^
        "  GC Collections: " ^ Int.toString collections
    end;

(* Force a garbage collection *)
fun forceGC () =
    let
        val _ = Gc.fullGC()
    in
        "Garbage collection performed"
    end;

(* ===== Test and Demonstration ===== *)

fun runTests () =
    let
        (* Test Stack implementation *)
        val _ = print "Testing Stack implementation...\n"
        val stack = Stack.create()
        val _ = Stack.push stack 10
        val _ = Stack.push stack 20
        val _ = Stack.push stack 30
        val top = Stack.peek stack
        val _ = print ("  Top of stack: " ^ 
                       (case top of NONE => "empty" | SOME x => Int.toString x) ^ "\n")
        val popped = Stack.pop stack
        val _ = print ("  Popped value: " ^ 
                       (case popped of NONE => "empty" | SOME x => Int.toString x) ^ "\n")
        val _ = print ("  Stack size: " ^ Int.toString (Stack.size stack) ^ "\n")
        val _ = print ("  Stack contents: " ^ 
                      String.concatWith ", " (List.map Int.toString (Stack.toList stack)) ^ "\n\n")
        
        (* Test Heap implementation *)
        val _ = print "Testing Heap implementation...\n"
        val minHeap = Heap.create (fn (a:int, b:int) => a < b) 10
        val _ = List.app (Heap.insert minHeap) [23, 5, 10, 8, 3, 16, 12]
        val min = Heap.peek minHeap
        val _ = print ("  Minimum value: " ^ 
                       (case min of NONE => "empty" | SOME x => Int.toString x) ^ "\n")
        val extracted = Heap.extractMin minHeap
        val _ = print ("  Extracted min: " ^ 
                       (case extracted of NONE => "empty" | SOME x => Int.toString x) ^ "\n")
        val _ = print ("  Heap size: " ^ Int.toString (Heap.size minHeap) ^ "\n")
        val sorted = Heap.toSortedList minHeap
        val _ = print ("  Sorted elements: " ^ 
                      String.concatWith ", " (List.map Int.toString sorted) ^ "\n\n")
                      
        (* Stack vs Heap Memory Usage *)
        val _ = print "Stack vs Heap Memory Patterns...\n"
        
        (* Stack recursion demo *)
        val _ = print "  Running recursive stack demo with depth 1000...\n"
        val _ = recursiveStackDemo 1000
        
        (* Tail recursion comparison *)
        val _ = print "  Comparing factorial implementations with n=10...\n"
        val fact1 = factorial 10
        val fact2 = factorialTail 10
        val _ = print ("    Regular: " ^ Int.toString fact1 ^ 
                       ", Tail-recursive: " ^ Int.toString fact2 ^ "\n")
        
        (* List building comparison *)
        val _ = print "  Comparing list building strategies with n=100000...\n"
        val _ = print "    Building efficient list..."
        val efficientTime = Timer.checkCPUTimer (Timer.startCPUTimer())
        val list1 = buildListEfficient 100000
        val efficientTime' = Timer.checkCPUTimer (Timer.startCPUTimer())
        val _ = print ("    Efficient time: " ^ 
                       Time.toString (#usr efficientTime') ^ "s\n")
        
        (* Demonstrate garbage generation and collection *)
        val _ = print "\nMemory Management Demo...\n"
        
        (* Get initial stats *)
        val _ = print "  Initial memory stats:\n"
        val _ = print ("  " ^ getMemoryStats() ^ "\n")
        
        (* Generate some garbage *)
        val _ = print "\n  Generating garbage (list of size 1000000)...\n"
        val _ = generateGarbage 1000000
        
        (* Get post-allocation stats *)
        val _ = print "  Post-allocation memory stats:\n"
        val _ = print ("  " ^ getMemoryStats() ^ "\n")
        
        (* Force garbage collection *)
        val _ = print "\n  Forcing garbage collection...\n"
        val _ = forceGC()
        
        (* Get post-GC stats *)
        val _ = print "  Post-GC memory stats:\n"
        val _ = print ("  " ^ getMemoryStats() ^ "\n")
        
        (* Test lazy evaluation *)
        val _ = print "\nLazy Evaluation Demo...\n"
        val lazyInts = countFrom 1
        val first10 = take 10 lazyInts
        val _ = print ("  First 10 integers: " ^ 
                      String.concatWith ", " (List.map Int.toString first10) ^ "\n")
        
        (* String concatenation comparison *)
        val _ = print "\nString Concatenation Demo...\n"
        val strings = List.tabulate(1000, fn i => "str" ^ Int.toString i)
        
        val _ = print "  Testing efficient concatenation..."
        val t1 = Timer.checkCPUTimer (Timer.startCPUTimer())
        val effStr = concatStringsEfficient strings
        val t1' = Timer.checkCPUTimer (Timer.startCPUTimer())
        val _ = print ("    Efficient time: " ^ 
                       Time.toString (#usr t1') ^ "s\n")
        val _ = print ("    Result length: " ^ Int.toString (String.size effStr) ^ "\n")
        
        (* Demo counter with reference cells *)
        val _ = print "\nReference Cell Demo...\n"
        val counter = makeCounter()
        val _ = #increment counter ()
        val _ = #increment counter ()
        val _ = #increment counter ()
        val _ = print ("  Counter value: " ^ Int.toString (#current counter ()) ^ "\n")
        val _ = #decrement counter ()
        val _ = print ("  After decrement: " ^ Int.toString (#current counter ()) ^ "\n")
        val _ = #reset counter ()
        val _ = print ("  After reset: " ^ Int.toString (#current counter ()) ^ "\n")
        
        (* CPS recursion demo *)
        val _ = print "\nCPS Recursion Demo...\n"
        val depth = 10000
        val _ = print ("  Computing with deep recursion (n=" ^ Int.toString depth ^ ")...\n")
        val cpsTiming = Timer.checkCPUTimer (Timer.startCPUTimer())
        val cpsResult = deepRecursionCPS depth
        val cpsTiming' = Timer.checkCPUTimer (Timer.startCPUTimer())
        val _ = print ("    CPS result: " ^ Int.toString cpsResult ^ "\n")
        val _ = print ("    CPS time: " ^ Time.toString (#usr cpsTiming') ^ "s\n")
    in
        print "\nAll demos completed!\n"
    end;

(* Run all demos *)
val _ = runTests();
