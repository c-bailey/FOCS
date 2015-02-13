(* FoCS Spring 2015

   Homework 2 code


   Name: Chelsea Bailey

   Email: chelsea.bailey@students.olin.edu

   Comments:

 *)




(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states :   'a list;
    	       alphabet : char list;
	       start :    'a;
   	       delta :    ('a * char * 'a) list;
	       final :    'a list}


(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)


(*
 * Some error code
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

let transitionError (input) = 
  failwith ("DFA Error: Cannot transition on input "^(implode [input]))


(*
 * Some sample DFAs
 *
 *)


let isolatedBs =                (* language: all strings where every b *)
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "readb"; "sink"];
   start = "start";
   delta = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["start";"readb"]}


let ambn =                 (* language: strings of a's followed by b's *)
    {states = ["eata"; "eatb"; "sink"];
     alphabet = ['a'; 'b'];
     start = "eata";
     delta = [("eata", 'a', "eata");
              ("eata", 'b', "eatb");
              ("eatb", 'a', "sink");
              ("eatb", 'b', "eatb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final = ["eata"; "eatb"]}


let aStar =                    (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     (* language: all nonempty strings of b's *)
  {alphabet= ['a'; 'b'];
   states= ["start"; "ok"; "sink"];
   start= "start";
   delta = [("start", 'b', "ok");
            ("start", 'a', "sink");
            ("ok",    'b', "ok");
            ("ok",    'a', "sink");
            ("sink",  'b', "sink");
            ("sink",  'a', "sink")];
   final = ["ok"]}


let abaStar =              (* language: any number of ab's followed by a's *)
  {alphabet= ['a'; 'b'];
   states= ["astate"; "bstate"; "aonly"; "sink"];
   start= "astate";
   delta = [("astate", 'a', "bstate");
            ("astate", 'b', "sink");
            ("bstate", 'a', "aonly");
            ("bstate", 'b', "astate");
            ("aonly",  'a', "aonly");
            ("aonly",  'b', "sink");
            ("sink",   'a', "sink");
            ("sink",   'b', "sink")];
   final = ["astate"; "bstate"; "aonly"]}



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings (alphabet, n) = 
  let rec mapCons (c, ls) = 
    match ls with
      [] -> []
    | l::ls' -> (c::l)::mapCons(c,ls')  in
  let rec mapConsSet (alphabet, l) = 
    match alphabet with
      [] -> []
    | c::cs -> mapCons(c,l) @ mapConsSet(cs,l)  in
  let rec mapImplode (css) = 
    match css with
      [] -> []
    | (cs::css) -> (implode cs)::mapImplode(css)  in
  let rec strings' (n) = 
    if (n<=0) then
      [[]]
    else let s = strings'(n-1) in
      [] :: mapConsSet(alphabet,s)
  in 
    mapImplode(strings'(n))




(*
 *  isFinal : 'a dfa * 'a -> bool
 *
 *    isFinal(dfa,q) should return true if and only if 'q' is a final state
 *    in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let isFinal (dfa,state) = 
  let rec isFinalHelper (finals,s) =
    match finals with
      [] -> false
    | h::t -> if h = s then true else isFinalHelper(t,s)
  in isFinalHelper(dfa.final,state);;





(* 
 *  transition : 'a dfa * 'a * char -> 'a
 *
 *    transition(dfa,q,a) should return the state obtained by reading input
 *    symbol 'a' in state 'q' in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)



let transition (dfa,state,input) = 
  let rec checker(delta,state,input) =
    match delta with
      [] -> "nada"
    | h::t -> match h with
      (a,b,c) -> if a = state && b=input then c else checker(t,state,input)
  in checker(dfa.delta,state,input);;

   failwith "transition not implemented"



(*
 *  extendedTransition : 'a dfa * 'a * char list -> 'a
 *
 *    extendedTransition(dfa,q,cs) should return the state obtained by
 *    reading the list of input symbols in 'cs' from state 'q' in the DFA
 *    'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let rec extendedTransition (dfa, state, cs) = 
  match cs with
    [] -> state
  | h::t -> extendedTransition(dfa, transition(dfa,state,h),t);;
  failwith "extendedTransition not implemented"


(*
 *  accept : 'a dfa * string -> bool
 *
 *    accept(dfa,input) should return true if and only the input string
 *    'input' is accepted by the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let accept (dfa, input) = 
  isFinal(dfa,extendedTransition(dfa,dfa.start,explode(input)));;



(*
 * PLACE YOUR ANSWERS TO QUESTION 3 HERE
 *
 * Each of these should be a function of no argument
 * returning the DFA that is a solution to the question
 *
 *)

let dfaQuestion1a () =
  {alphabet = ['a'; 'b'];          
   states = ["start"; "one"; "two"; "three"; "sink"];
   start = "start";
   delta = [("start", 'a', "one");
            ("start", 'b', "one");
            ("one", 'a', "two");
            ("one", 'b', "two"); 
            ("two", 'a', "three");
            ("two", 'b', "three"); 
            ("three", 'a', "sink");
            ("three", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["three"]};;

let dfaQuestion1b () = 
  {alphabet = ['a'; 'b'];          
   states = ["start"; "one"; "two"; "three"];
   start = "start";
   delta = [("start", 'a', "one");
            ("start", 'b', "one");
            ("one", 'a', "two");
            ("one", 'b', "two"); 
            ("two", 'a', "three");
            ("two", 'b', "three"); 
            ("three", 'a', "one");
            ("three", 'b', "one")]; 
   final = ["three"]};;

let dfaQuestion1c () = 
  {alphabet = ['a'; 'b'];          
   states = ["start"; "reada";];
   start = "start";
   delta = [("start", 'a', "reada");
            ("start", 'b', "start");
            ("reada", 'a', "start");
            ("reada", 'b', "reada")];  
   final = ["reada"]};;

let dfaQuestion1d () = 
  {alphabet = ['a'; 'b'];          
   states = ["start"; "readb";"sink"];
   start = "start";
   delta = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];  
   final = ["start"]};;

let dfaQuestion1e () = 
  {alphabet = ['a'; 'b'];          
   states = ["start"; "one"; "two"; "three"; "four"; "five"; "six"];
   start = "start";
   delta = [("start", 'a', "one");
            ("start", 'b', "one");
            ("one", 'a', "two");
            ("one", 'b', "two"); 
            ("two", 'a', "three");
            ("two", 'b', "three"); 
            ("three", 'a', "four");
            ("three", 'b', "four"); 
            ("four", 'a', "five");
            ("four", 'b', "five");
            ("five", 'a', "six");
            ("five", 'b', "six");
            ("six", 'a', "one");
            ("six", 'b', "one")];
   final = ["two";"three";"four";"six"]};;




(* 
 *  Compute the language of a DFA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *)

let language (dfa, n) = 
  let candidates = strings(dfa.alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (accept(dfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)


let printLanguage (dfa,n) = 
  let rec printList (l) = 
    match l with 
      [] -> ()
    | s::ss -> (print_string "   ";
                if (s="") then
                  print_string "<empty>"
                else
                  print_string s; 
                print_newline(); 
                printList ss)
  in
    printList(language(dfa,n))

