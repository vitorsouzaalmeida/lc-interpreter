(* type term = Var of int | VarFree of string | Abs of term | App of term * term

(* let rec to_string t =
  match t with
  | Var x -> string_of_int x
  | VarFree x -> x
  | Abs t -> "λ." ^ to_string t
  | App (t1, t2) -> "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")" *)

let rec count_app t =
  match t with
  | Var 0 -> 0
  | App (Var 1, rest) -> 1 + count_app rest
  | _ -> failwith "its not a church numeral body"

let _decode_bool t =
  match t with
  | Abs (Abs (Var 1)) -> true
  | Abs (Abs (Var 0)) -> false
  | _ -> failwith "not a church boolean bro"

let decode_num t =
  match t with
  | Abs (Abs body) -> count_app body
  | _ -> failwith "not a church numeral bro"

let rec shift d cutoff t =
  match t with
  | Var k -> if k >= cutoff then Var (k + d) else Var k
  | VarFree s -> VarFree s
  | Abs a ->
      let nc = cutoff + 1 in
      Abs (shift d nc a)
  | App (t1, t2) -> App (shift d cutoff t1, shift d cutoff t2)

let rec subst j s t =
  match t with
  | Var i -> if i = j then s else Var i
  | Abs t' ->
      let j' = j + 1 in
      Abs (subst j' (shift 1 0 s) t')
  | App (t1, t2) -> App (subst j s t1, subst j s t2)
  | VarFree x -> VarFree x

let rec reduce t =
  match t with
  | App (Abs body, arg) -> shift (-1) 0 (subst 0 (shift 1 0 arg) body)
  | App (t1, t2) ->
      let t1' = reduce t1 in
      if t1 <> t1' then App (t1', t2) else App (t1, reduce t2)
  | Abs t' -> Abs (reduce t')
  | _ -> t

let rec normalize t =
  let t' = reduce t in
  if t = t' then t else normalize t'

let () =
  let _zero = Abs (Abs (Var 0)) in
  let _one = Abs (Abs (App (Var 1, Var 0))) in
  let two = Abs (Abs (App (Var 1, App (Var 1, Var 0)))) in
  let succ = Abs (Abs (Abs (App (Var 1, App (App (Var 2, Var 1), Var 0))))) in
  let three = decode_num (normalize (App (succ, two))) in
  print_int three *)

type term = Var of int | Abs of term | App of term * term

let rec to_string ~term =
  match term with
  | Var i -> string_of_int i
  | Abs t -> "λ." ^ to_string ~term:t
  | App (t1, t2) -> "(" ^ to_string ~term:t1 ^ " " ^ to_string ~term:t2 ^ ")"

let rec shift ~by ~depth ~term =
  match term with
  | Var i -> if i >= depth then Var (i + by) else Var i
  | Abs t ->
      let depth' = depth + 1 in
      Abs (shift ~by ~depth:depth' ~term:t)
  | App (t1, t2) -> App (shift ~by ~depth ~term:t1, shift ~by ~depth ~term:t2)

let () =
  let ex = Abs (Abs (App (Var 2, Var 1))) in
  print_endline (to_string ~term:ex)
(* 
let rec free_in ~variable ~term =
  match term with
  | Var x -> x = variable
  | App (lt, rt) -> free_in ~variable ~term:lt || free_in ~variable ~term:rt
  | Abs (x, body) ->
      if x = variable then false else free_in ~variable ~term:body

let fresh_var =
  let counter = ref 0 in
  fun base ->
    incr counter;
    base ^ string_of_int !counter

let rec subst ~in_term ~variable ~by_term =
  match in_term with
  | Var v -> if v = variable then by_term else Var v
  | App (t1, t2) ->
      App
        ( subst ~in_term:t1 ~variable ~by_term,
          subst ~in_term:t2 ~variable ~by_term )
  | Abs (x, t) ->
      if x = variable then Abs (x, t)
      else if free_in ~variable:x ~term:by_term then
        let x' = fresh_var x in
        let t' = subst ~in_term:t ~variable:x ~by_term:(Var x') in
        Abs (x', subst ~in_term:t' ~variable ~by_term)
      else Abs (x, subst ~in_term:t ~variable ~by_term)

let rec reduce ~term =
  match term with
  | App (Abs (var, body), arg) -> subst ~in_term:body ~variable:var ~by_term:arg
  | App (lt, rt) ->
      let lt' = reduce ~term:lt in
      if lt <> lt' then App (lt', rt) else App (lt, reduce ~term:rt)
  | Abs (var, body) -> Abs (var, reduce ~term:body)
  | Var _ -> term

let () =
  let term = App (Abs ("x", Abs ("y", Var "x")), Var "y") in
  let result = to_string ~term:(reduce ~term) in
  print_endline result *)
