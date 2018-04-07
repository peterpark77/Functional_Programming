module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Peter Park, Id Number: 260571481 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *) 

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

let rec newton(f,guess:float,tol:float,dx:float) = 
   if abs (f guess) < tol then guess
   else newton (f, guess-  (f guess) / (deriv (f,dx) guess) , tol, dx)


let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)

let root = newton(sin,5.0,0.0001,0.0001)


(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm(Term (a,b):term, Poly p:poly):poly =
    
    if (List.isEmpty p) then raise EmptyList
    else 
      let rec helper (Term (l1,l2), Poly term) = 
        match term with
        | [] -> []
        | (x,y)::xs -> (x*l1,y+l2)::helper(Term (l1,l2), Poly xs)
    
      Poly (helper (Term (a,b), Poly p))
    

let addTermToPoly(Term (c,e):term, Poly p:poly):poly = 

  if (List.isEmpty p) then raise EmptyList
  else
    let rec helpher(Term (a,b), Poly term) = 
      match term with
      | [] -> []
      | (x,d)::xs -> if (d = b) then (x+a,d)::xs
                      elif (d > b) then (x,d)::(helpher (Term (a,b), Poly xs))
                      else (a,b)::((x,d)::xs)
    Poly (helpher (Term (c,e), Poly p))

let addPolys(Poly p1:poly, Poly p2:poly):poly =

  if (List.isEmpty p1) then raise EmptyList
  elif (List.isEmpty p2) then raise EmptyList
  else
    let rec help(Poly input1, Poly input2) =
      match input1 with
      | [] -> input2
      | (x,y)::xs -> help (Poly xs, addTermToPoly (Term (x,y), Poly input2))
    Poly (help (Poly p1, Poly p2))


let multPolys(Poly p1:poly, Poly p2:poly) =

  if (List.isEmpty p1) then raise EmptyList
  elif (List.isEmpty p2) then raise EmptyList
  else
    let rec helpme(Poly input1, Poly input2) =
      match input1 with
      |[]-> input2
      |(x,y)::xs-> helpme (Poly xs, multiplyPolyByTerm (Term (x,y), Poly input2))
    Poly (helpme (Poly p1, Poly p2))


let exp(b:float, e:int) =
  let rec helper(input1:float, input2:int, input3: float) =
    if (input1 = 0.0) then 0.0
    elif (input2 = 0) then input3
    elif (input2 % 2 = 1) then helper(input1,input2-1, input1*input3)
    else helper(input1*input1,input2/2,input3)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float = 
  
  if (List.isEmpty p) then raise EmptyList
  else
    let rec helperfunction2(Poly input1, v:float) = 
      match input1 with
      | [] -> 0.0
      | (x,y)::xs -> (evalTerm v (Term (x,y))) + (helperfunction2 (Poly xs, v))
    helperfunction2 (Poly p, v)

let diffPoly (Poly p) = 

  if (List.isEmpty p) then raise EmptyList
  else
    let rec helperlast(Poly input1) =
      match input1 with
      | [] -> []
      | (x,y)::xs -> if (y = 0) then []
                      else (x*(float y),y-1)::helperlast(Poly xs)
    Poly (helperlast (Poly p))

(* Question 3 *)
type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

let stringCompare(s1:string, s2:string) =
  s1>s2

let rec lookup(name:string, env: Bindings) = 

  match env with
  |[] -> None
  |(st,n)::xs -> if (stringCompare(name,st)) then lookup (name, xs)
                  elif (name = st) then Some n
                  else None


let rec insert(name:string, value: int, b: Bindings) = 

  match b with
  | [] -> [(name,value)]
  | (a,c)::xs -> if (stringCompare(name,a)) then (a,c)::insert(name, value, xs)
                  else (name,value)::((a,c)::xs)
                                           
let rec eval(exp : Exptree, env:Bindings) =

  match exp with
  | Const num -> Some num
  | Var x -> match (lookup (x,env)) with
              | Some t1 -> Some t1
              | None -> None
  | Add (input1, input2) ->
      let u1 = eval (input1,env)
      let u2 = eval (input2,env)
      match u1 with
      | Some y -> match u2 with
                  | Some t -> Some (t+y)
                  | None -> None
      | None -> None
  | Mul (input1, input2) ->
      let u1 = eval (input1,env)
      let u2 = eval (input2,env)
      match u1 with
      | Some y -> match u2 with
                  | Some t -> Some (t*y)
                  | None -> None
      | None -> None

let env:Bindings = [("a",3);("b",4);("c",5)]                                

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)


(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

let pointsMade (r: Result) = 
  match r with
  |(a,b),(c,d) -> if b>d then ((a,Points 3),(c,Points 0)) elif b=d then ((a,Points 1),(c,Points 1)) else ((a,Points 0),(c,Points 3))


let initEntry (name:Team) = (name, Points 0)
           
let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table = 
  let ((a,b),(c,d)) = pointsMade r
  let l = Map.toList t
  let rec helper t =
    match t with
    | [] -> []
    | (x,l)::xs -> if (x = a) then match l with
                                    | Points z -> match b with
                                                   | Points y -> (x, Points (z+y))::(helper xs)
                    elif (x = c) then match l with
                                        | Points z -> match d with
                                                       | Points y -> (x, Points (z+y))::(helper xs)
                    else (x,l)::(helper xs)
  let updated = helper l
  Map.ofList updated

let rec weekendUpdate(t:Table,rl: Result list): Table = 
   match t,rl with
    | (t,[]) -> t
    | t,x::xs -> weekendUpdate(updateTable(t,x),xs)

let rec seasonUpdate(t:Table, sll:Result list list) : Table = 
   match t,sll with
    | (t,[]) -> t
    | t,x::xs -> seasonUpdate(weekendUpdate(t,x),xs)


let less((s1,n1):Team * Points, (s2,n2):Team * Points) = 
  if (n1 > n2) then s1 > s2 else s2 <= s1
let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)
                                                  
(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]

let makeRoadMap data = 

  let makeRightMap (source, destination) = (City source, Set.ofList (List.map City destination))
  let rec makeAllRight input =
    match input with
    | [] -> []
    | (x,y)::xs -> makeRightMap(x,y)::(makeAllRight xs)
  Roads (Map.ofList (makeAllRight data))

let rec upToManySteps (Roads r) n startCity = 
  let adjCities = Map.find startCity r
  match n with
  | 1 -> adjCities
  | o -> Set.fold (fun acc y -> (Set.union (upToManySteps (Roads r) (o-1) y) acc)) adjCities adjCities

