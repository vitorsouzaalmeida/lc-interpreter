type term = Var of int | VarFree of string | Abs of term | App of term * term

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
  print_int three
