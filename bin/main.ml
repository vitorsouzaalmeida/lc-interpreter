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

let rec subst ~index ~arg ~term =
  match term with
  | Var i -> if i = index then arg else Var i
  | Abs t ->
      let index = index + 1 in
      Abs (subst ~index ~arg:(shift ~by:1 ~depth:0 ~term:arg) ~term:t)
  | App (t1, t2) -> App (subst ~index ~arg ~term:t1, subst ~index ~arg ~term:t2)

let rec reduce ~term =
  match term with
  | App (Abs body, arg) ->
      shift ~by:(-1) ~depth:0
        ~term:(subst ~index:0 ~arg:(shift ~by:1 ~depth:0 ~term:arg) ~term:body)
  | App (t1, t2) ->
      let t1' = reduce ~term:t1 in
      if t1 <> t1' then App (t1', t2) else App (t1, reduce ~term:t2)
  | Abs t -> Abs (reduce ~term:t)
  | _ -> term

let rec normalize ~term =
  let t = reduce ~term in
  if term = t then term else normalize ~term:t

let () =
  let two = Abs (Abs (App (Var 1, App (Var 1, Var 0)))) in
  let succ = Abs (Abs (Abs (App (Var 1, App (App (Var 2, Var 1), Var 0))))) in
  print_endline (to_string ~term:(normalize ~term:(App (succ, two))))
