type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool =
  match t with
  | Const _ -> false
  | Var a -> x = a
  | Term (_, b) -> List.exists (occurs x) b
(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : term) (x : id) (t : term) : term =
  match t with
  | Const x -> Const x
  | Var a -> if x = a then s else t
  | Term (b, c) -> Term (b, List.map (subst s x) c)

(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term) : term =
  List.foldBack (fun (a, b) -> subst b a) s t

(* unify one pair *)
let rec unify (s : term) (t : term) : substitution =
    match (s, t) with
    | (Var a, Var b) -> if (a = b) then [] else [(a,t)]
    | (Term (c,xx), Term (d,yy)) -> if (c = d) && (List.length xx) = (List.length yy)
                                    then unify_list (List.zip xx yy)
                                    else failwith "not unifiable: head symbol conflict"
    | ((Var a, (Term _ as te)) | ((Term _ as te), Var a)) -> if occurs a te
                                                              then failwith "not unifiable: circularity"
                                                              else [(a, te)]
    | (((Const a), (Var b)) | ((Var b), (Const a))) -> [(b, Const a)]
    | (Const a, Const b) -> if a = b then [] else failwith "not unifiable: clashing constants"
    | ((Const _,Term _) | (Term _,Const _)) -> failwith "not unifiable: term constant clash"
    


(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution =
    match s with
    | [] -> []
    | (a,b)::c -> let t1 = unify_list c in
                    let t2 = unify (apply t1 a) (apply t1 b) in
                    t2@t1

(*
Examples
> let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
val t1 : term = Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])
> let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
val t2 : term =
  Term ("f",[Term ("g",[Var "z"]); Term ("h",[Var "x"]); Var "y"])
> let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
val t3 : term = Term ("f",[Var "x"; Var "y"; Term ("g",[Var "u"])])
> unify t1 t2;;
val it : substitution =
  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))]
> let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
val t4 : term = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])
>  let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
val t5 : term = Term ("f",[Term ("k",[Var "y"]); Var "y"; Var "x"])
> unify t4 t5;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> unify t5 t4;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> apply it t4;;
val it : term =
  Term
    ("f",
     [Term ("k",[Term ("h",[Var "z"])]); Term ("h",[Var "z"]);
      Term ("k",[Term ("h",[Var "z"])])])
> let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
val t6 : term = Term ("f",[Const 2; Var "x"; Const 3])
> let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
val t7 : term = Term ("f",[Const 2; Const 3; Var "y"])
> unify t6 t7;;
val it : substitution = [("x", Const 3); ("y", Const 3)]
> apply it t7;;
val it : term = Term ("f",[Const 2; Const 3; Const 3])
> unify t1 t7;;
System.Exception: not unifiable: term constant clash
....... junk removed .............
Stopped due to error
*)