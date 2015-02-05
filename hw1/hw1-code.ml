(* FoCS Spring 2015

   Homework 1 code


   Name: Chelsea Bailey

   Email: chelsea.bailey@students.olin.edu

   Comments: This homework was the most fun I've had all week! It was enjoyable and went by pretty quick ^_^ looking forward to more!

 *)



(* 
 *  Question 1
 *)

let rec append (xs,ys) =  
  match xs with
    [] -> ys
  | h::t -> h::append(t,ys);;


let rec flatten (xs) = 
  match xs with
    [] -> []
  | h::t -> append(h,flatten(t));;

let rec double (xs) = 
  match xs with
    [] -> []
  | h::t -> 2*h::double(t);;

let rec last (xs) = 
  match xs with
    [] -> None
  | h::t -> if t = [] then Some h else last(t);; 



(*
 *  Question 2 
 *)

let rec setIn (elt,set) = 
  match set with
    [] -> false
  | h::t -> if elt = h then true else setIn(elt,t);;

let rec setSub (set1,set2) = 
  match set1 with
    [] -> true
  | h::t -> if not setIn(h,set2) then false else setSub(t,set2);;

let setEqual (set1,set2) = 
  match set1 with
    [] -> if set2 = [] then true else false
  | h::t -> if setSub(set1,set2) && setSub(set2,set1) then true else false;;

let setUnion (set1,set2) = 
  match set1 with
    [] -> set2
  | h::t -> append(set1,set2);;

let setInter (set1,set2) = 
  let rec setInterHelper (set1,set2,set3) =
    (match set1 with
      [] -> set3
    | h::t -> if setIn(h,set2) then setInterHelper(t,set2,h::set3) else setInterHelper (t,set2,set3))
  in setInterHelper (set1,set2,[]);;


let setSize (set) =
  let rec counter (set1, set2, n) =
    match set1 with
      [] -> n
    | h::t -> if setIn(h,set2) then counter(t,set2,n) else counter(t,h::set2,n+1)
  in counter (set,[],0);;


(* 
 *  Question 3
 *)

type rat = {num: int; den: int}

let half = {num=1; den=2}
let third = {num=1; den=3}
let fourth = {num=1; den=4}

let floatR (r) = float(r.num) /. float(r.den)

let rec gcd (a,b) =
  if b = 0 then a else gcd(b,a mod b);;

let simplify (r) = 
  let divid = gcd(abs(r.num),abs(r.den)) in
  {num = r.num/divid; den = r.den/divid};;

let addR (r1,r2) = 
  simplify({num = (r1.num*r2.den) + (r2.num*r1.den); den = r1.den*r2.den});;

let multR (r1,r2) = 
  simplify({num = r1.num*r2.num; den = r1.den*r2.den});;

type number = I of int
            | R of rat
            | F of float

let add (n1,n2) = 
  match (n1,n2) with
    (I i, I j) -> I (i+j)
  | (I i, F f) -> F (float(i) +. f)
  | (I i, R r) -> R (addR (r,{num = i; den = 1}))
  | (F f, F g) -> F (f+.g)
  | (F f, I j) -> F (f +. float(j))
  | (F f, R r) -> F (f +. floatR(r))
  | (R r, R s) -> R (addR (r,s))
  | (R r, I i) -> R (addR (r,{num = i; den = 1}))
  | (R r, F f) -> F (floatR(r) +. f);;



(* 
 *  Optional question
 *)


type bConst = True | False

type bExpr = Constant of bConst
           | Variable of string
           | And of bExpr * bExpr
           | Or of bExpr * bExpr
           | Not of bExpr

let sample1 = And(Not(Variable "a"),Not(Variable "b"))

let sample2 = Or(Not(Variable "a"),And(Variable "b",Constant(True)))

let sample3 = And(Variable "a", Not(Variable "a"))

let rec vars (bexpr) = 
  match bexpr with
    Constant c -> []
  | Variable v -> [v]
  | And (a,b) -> append(vars(a),vars(b))
  | Or (a,b) -> append(vars(a),vars(b))
  | Not n -> vars(n);;

let rec subst (bexpr,var,sub) = 
  match bexpr with
    Constant c -> Constant c
  | Variable v -> if v = var then sub else Variable v
  | And (a,b) -> And (subst(a,var,sub),subst(b,var,sub))
  | Or (a,b) -> Or (subst(a,var,sub),subst(b,var,sub))
  | Not n -> Not (subst(n,var,sub));;

let toBool (bconstopt) =
  match bconstopt with
  | Some True -> true
  | Some False -> false;;

let toBcon (boo) =
  match boo with
    true -> Some True
  | false -> Some False;;

let rec eval (bexpr) = 
  match bexpr with
    Constant c -> Some c
  | Variable v -> None
  | And (a,b) -> if eval(a) = None || eval(b) = None then None else toBcon( toBool(eval(a)) && toBool(eval(b)) )  
  | Or (a,b) -> if eval(a) = None || eval(b) = None then None else toBcon( toBool(eval(a)) || toBool(eval(b)) ) 
  | Not n -> if eval(n) = None then None else if eval(n) = Some True then Some False else Some True;;
