(* FoCS Spring 2015

   Homework 3 code


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


let aStar =                    
  (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     
  (* language: all nonempty strings of b's *)
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
 * REPLACE THIS WITH YOUR IMPLEMENTATION OF 
 * accept from homework 2 if you want to test
 * your DFA code
 *
 *)

let isFinal (dfa,state) = 
  let rec isFinalHelper (finals,s) =
    match finals with
      [] -> false
    | h::t -> if h = s then true else isFinalHelper(t,s)
  in isFinalHelper(dfa.final,state);;

let transition (dfa,state,input) = 
  let rec checker(delta,state,input) =
    match delta with
      [] -> "nada"
    | h::t -> match h with
      (a,b,c) -> if a = state && b=input then c else checker(t,state,input)
  in checker(dfa.delta,state,input);;



let rec extendedTransition (dfa, state, cs) = 
  match cs with
    [] -> state
  | h::t -> extendedTransition(dfa, transition(dfa,state,h),t);;


let accept (dfa, input) = 
  isFinal(dfa,extendedTransition(dfa,dfa.start,explode(input)));;



(*
 * Exercise 2 
 *
 *)

let rec compHelper (dfa,states,finals) =
    match states with 
      [] -> finals
    | h::t -> if isFinal(dfa,h) then compHelper(dfa,t,finals) else compHelper(dfa,t,h::finals);;

let complement (dfa) = 
  {alphabet = dfa.alphabet;          
   states = dfa.states;
   start = dfa.start;
   delta = dfa.delta; 
   final = compHelper(dfa,dfa.states,[])};;


let rec pairing (li, el, fin) =
  match li with
    [] -> fin
  | h::t -> pairing(t,el,(h,el)::fin);;

let cross (xs, ys) = 
  let rec helpcross (xs, ys, fin) =
    match xs with
      [] -> fin
    | h::t -> helpcross(t,ys,pairing(ys,h,fin)) in
  helpcross(ys,xs,[]);;


let union (dfa1, dfa2) = 
  {states = cross(dfa1.states,dfa2.states);
   alphabet = cross(dfa1.alphabet,dfa2.alphabet);          
   start = cross(dfa1.start,dfa2.start);
   delta = cross(dfa1.delta,dfa2.delta); 
   final = cross(dfa1.final,dfa2.final)};;

(* type 'a dfa = {states :   'a list;
             alphabet : char list;
         start :    'a;
           delta :    ('a * char * 'a) list;
         final :    'a list} *)


(*
 *  Set-based helper functions, mostly from Homework 1
 *
 *  The only addition is 'subsets', which computes the
 *  set of subsets of a set (all taken to be lists, 
 *  of course)
 *
 *  'subsets' should be handy for Exercise 4
 *
 *)


let rec setIn (elt,set) = 
  match set with
    [] -> false
  | x::xs -> (x = elt || setIn(elt,xs))

let rec setSub (set1,set2) = 
  match set1 with
    [] -> true
  | x::xs -> setIn(x,set2) && setSub(xs,set2)

let setEqual (set1,set2) = 
  setSub(set1,set2) && setSub(set2,set1)

let rec setInter (set1,set2) = 
   match set1 with 
     [] -> []
   | x::xs -> if setIn(x,set2) then 
                 x::(setInter(xs,set2))
              else 
                 setInter(xs,set2)

let rec subsets xs = 
  match xs with
    [] -> [[]]
  | x::xs' -> subsets(xs') @ (List.map (fun ys -> x::ys) (subsets xs'))




(* 
 * The type for an NFA, parameterized by the type for the states 
 *
 *)

type 'a nfa = {nfa_states :   'a list;
               nfa_alphabet : char list;
               nfa_start :    'a;
   	       nfa_delta :    ('a * char * 'a list) list;
	       nfa_final :    'a list}


(*
 * Some sample NFAs
 *
 *)


(* language: (ab+aab)* *)
let abaabStar = {nfa_states = ["s";"a11";"a21";"a22"];
                 nfa_alphabet = ['a';'b'];
                 nfa_start = "s";
                 nfa_delta = [("s", 'a', ["a11"; "a21"]);
                               ("a21", 'a', ["a22"]);
                               ("a11", 'b', ["s"]);
                               ("a22", 'b', ["s"])];
                 nfa_final = ["s"]}

(* language: ab+aab *)
let abaab = {nfa_states = ["q1"; "q2"; "q3"; "q4"; "q5"];
             nfa_alphabet = ['a'; 'b'];
             nfa_start = "q1";
             nfa_delta = [("q1", 'a', ["q2"; "q3"]);
                        ("q2", 'b', ["q5"]);
                        ("q3", 'a', ["q4"]);
                        ("q4", 'b', ["q5"])];
             nfa_final = ["q5"]}





(*
 * In common for exercises 3 and 4
 *
 *)


let nfa_hasFinal (nfa,states) = 
  failwith "nfa_hasFinal not implemented"

let nfa_transition (nfa,states,input) = 
  failwith "nfa_transition not implemented"



(*
 * Exercise 3
 *
 *)


let nfa_extendedTransition (nfa, states, cs) = 
  failwith "nfa_extendedTransition not implemented"


let nfa_accept (nfa, input) = 
  failwith "nfa_accept not implemented"




(*
 * Exercise 4
 *
 *)


let subsetConstruction nfa =
  failwith "subsetConstruction not implemented"




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


(* 
 *  Compute the language of an NFA, restricted to inputs of length <= n
 *   nfa_language(nfa,n) returns a list of strings accepted by nfa
 *   nfa_printLanguage(nfa,n) prints the strings accepted by nfa
 *
 *)

let nfa_language (nfa, n) = 
  let candidates = strings(nfa.nfa_alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (nfa_accept(nfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)

let nfa_printLanguage (nfa,n) = 
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
    printList(nfa_language(nfa,n))
