(* β-reduction rule: (λx. M) N  →  M[x := N] 
   in the body M, replace every free x with N.
   Examples:
     (λx.x) y
       → x[x := y]
       → y

     (λx.x) (λy.y)
       → x[x := (λy.y)]
       → (λy.y)

     (λx.λy.x) z
       → (λy.x)[x := z]
       → (λy.z)

     ((λx.λy.x) z) w
       → (λy.z) w
       → z

     ((λx.λy.y) z) w
       → (λy.y) w
       → w
*)

(* A lambda calculus term is either:
   - Var "x"          a variable
   - Abs ("x", body)  an abstraction λx.body (a function definition)
   - App (t1, t2)     an application (function call): (t1 t2) *)
type term = Var of string | Abs of string * term | App of term * term

(* Convert the AST back to a lambda expression.
   - Variables → just their name
   - Abstractions → "λx." + body
   - Applications → put parentheses around (t1 t2) *)
let rec to_string t =
  match t with
  | Var x -> x
  | Abs (x, body) -> "λ" ^ x ^ "." ^ to_string body
  | App (t1, t2) -> "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")"

(* Needed to avoid "variable capture" during substitution.
   fresh_var "y" → "y1"
   fresh_var "y" again → "y2" *)
let fresh_var =
  let counter = ref 0 in
  fun base ->
    incr counter;
    base ^ string_of_int !counter

(* free variable check
   free_in x t = true if variable x occurs free in term t. *)
let rec free_in x t =
  match t with
  | Var y ->
      (* A variable is free iff it matches x *)
      y = x
  | App (t1, t2) ->
      (* Free if x is free in either side *)
      free_in x t1 || free_in x t2
  | Abs (y, body) ->
      if y = x then
        (* λx.body: here x is bound, so it's not free inside *)
        false
      else
        (* Different binder: recurse into body *)
        free_in x body

(* β-reduction 
   subst body x replacement
   = in 'body', replace free occurrences of x with 'replacement' *)
let rec subst body x replacement =
  match body with
  | Var y ->
      (* If variable matches, replace it. Otherwise leave it *)
      if y = x then replacement else Var y
  | App (t1, t2) ->
      (* Recurse into both sides of the application *)
      App (subst t1 x replacement, subst t2 x replacement)
  | Abs (y, t) ->
      if y = x then
        (* Case: λx.t. This λ shadows x, so stop *)
        Abs (y, t)
      else if free_in y replacement then
        (* Risk of capture: do α-renaming of y *)
        let y' = fresh_var y in
        let t' = subst t y (Var y') in
        Abs (y', subst t' x replacement)
      else
        (* Safe to recurse inside abstraction *)
        Abs (y, subst t x replacement)

(* Implements β-reduction + recursive traversal *)
let rec reduce t =
  match t with
  | App (Abs (x, body), arg) ->
      (* Direct β-step: (λx.body) arg → body[x := arg] *)
      subst body x arg
  | App (t1, t2) ->
      (* Try reducing the left side first *)
      let t1' = reduce t1 in
      if t1 <> t1' then App (t1', t2) else App (t1, reduce t2)
  | Abs (x, body) ->
      (* Recurse inside abstractions *)
      Abs (x, reduce body)
  | Var _ ->
      (* Variables don’t reduce *)
      t

(* keep reducing until no more changes *)
let rec normalize t =
  let t' = reduce t in
  if t = t' then t else normalize t'

let () =
  (* Identity applied once
     (λx.x) y → y *)
  print_endline
    ("Identity applied once: "
    ^ to_string (normalize (App (Abs ("x", Var "x"), Var "y"))));

  (* Identity applied twice
     (λx.x) ((λy.y) z) → z *)
  print_endline
    ("Identity applied twice: "
    ^ to_string
        (normalize
           (App (Abs ("x", Var "x"), App (Abs ("y", Var "y"), Var "z")))));

  (* K combinator
     K = λx.λy.x
     Example: (K a b) → a *)
  let k = Abs ("x", Abs ("y", Var "x")) in
  print_endline ("K combinator itself: " ^ to_string k);
  print_endline
    ("K applied to a and b: "
    ^ to_string (normalize (App (App (k, Var "a"), Var "b"))));

  (* S combinator variant
     S = λf.λg.λx. f x (g x)
     Example: (S I I z) → (z z) *)
  let s =
    Abs
      ( "f",
        Abs
          ("g", Abs ("x", App (App (Var "f", Var "x"), App (Var "g", Var "x"))))
      )
  in
  let i = Abs ("x", Var "x") in
  print_endline
    ("S applied to I I z: "
    ^ to_string (normalize (App (App (App (s, i), i), Var "z"))));

  (* Boolean logic
     true  = λt.λf.t
     false = λt.λf.f
     if b t f = b t f
     Example: if true then a else b → a *)
  let tru = Abs ("t", Abs ("f", Var "t")) in
  let ifthenelse =
    Abs ("b", Abs ("t", Abs ("f", App (App (Var "b", Var "t"), Var "f"))))
  in
  print_endline
    ("If true then a else b: "
    ^ to_string
        (normalize (App (App (App (ifthenelse, tru), Var "a"), Var "b"))));

  (* Example with α-renaming
     (λx.λy.x) y → λz.y *)
  print_endline
    ("Alpha-renaming example: "
    ^ to_string (normalize (App (Abs ("x", Abs ("y", Var "x")), Var "y"))))
