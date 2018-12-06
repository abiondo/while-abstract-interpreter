module L = Language

(* Module for the program state *)
module State = struct
	include Map.Make(struct
		type t = L.var
		let compare = compare
	end)

	let eval_var (x : L.var) (s : L.value t) : L.value =
		match find_opt x s with
		| None     -> failwith ("Unbound variable " ^ x)
		| Some (v) -> v
end

(* Type for the program state *)
type state = L.value State.t

(* Type for total semantic functions *)
type sem_func = state -> state
(* Type for partial semantic functions *)
type sem_func_partial = state -> state option

(* Identity partial semantic function *)
let id (s : state) : state option = Some (s)

(* Evaluates an arithmetic expression *)
let rec eval_a_expr (a : L.a_expr) (s : state) : L.value =
	match a with
	| Num (n)      -> n
	| Var (x)      -> State.eval_var x s
	| Sum (a1, a2) -> (eval_a_expr a1 s) + (eval_a_expr a2 s)
	| Sub (a1, a2) -> (eval_a_expr a1 s) - (eval_a_expr a2 s)
	| Mul (a1, a2) -> (eval_a_expr a1 s) * (eval_a_expr a2 s)

(* Evaluates a boolean expression *)
let rec eval_b_expr (b : L.b_expr) (s : state) : bool =
	match b with
	| Bool (b1)     -> b1
	| Not  (b1)     -> not (eval_b_expr b1 s)
	| And  (b1, b2) -> (eval_b_expr b1 s) && (eval_b_expr b2 s)
	| Or   (b1, b2) -> (eval_b_expr b1 s) || (eval_b_expr b2 s)
	| Eq   (a1, a2) -> (eval_a_expr a1 s) == (eval_a_expr a2 s)
	| Ne   (a1, a2) -> (eval_a_expr a1 s) != (eval_a_expr a2 s)
	| Le   (a1, a2) -> (eval_a_expr a1 s) <= (eval_a_expr a2 s)
	| Ge   (a1, a2) -> (eval_a_expr a1 s) >= (eval_a_expr a2 s)
	| Lt   (a1, a2) -> (eval_a_expr a1 s) < (eval_a_expr a2 s)
	| Gt   (a1, a2) -> (eval_a_expr a1 s) > (eval_a_expr a2 s)

(* Composition of partial semantic functions *)
let (%.) (f : sem_func_partial) (g : sem_func_partial) (s : state) : state option =
	match g s with
	| None      -> None
	| Some (ss) -> f ss

(* Polymorphic conditional function *)
let cond (b : 'a -> bool) (f1 : 'a -> 'b) (f2 : 'a -> 'b) (x : 'a) : 'b =
	(if b x then f1 else f2) x

(* Turns a total function into an always-defined partial function *)
let partial (f : 'a -> 'b) (x : 'a): 'b option = Some (f x)

(* Auxiliary function whose fixpoint is the while semantic function *)
let while_aux (b : state -> bool) (sm : sem_func) (g : sem_func_partial) : sem_func_partial =
	cond b (g %. partial sm) id

(* Auxiliary function whose fixpoint is the repeat-until semantic function *)
let repeat_aux (b : state -> bool) (sm : sem_func) (g : sem_func_partial) : sem_func_partial =
	cond b id g %. partial sm

(* Semantic function for a statement *)
let rec semantic (st : L.stm) (s : state) : state =
	match st with
	| Skip                    -> s
	| Assign (x, a)           -> State.add x (eval_a_expr a s) s
	| Comp   (st1, st2)       -> semantic st2 @@ semantic st1 s
	| If     (b, st1, st2)    -> cond (eval_b_expr b) (semantic st1) (semantic st2) s
	| While  (b, st1)         -> Ccpo.fix (while_aux (eval_b_expr b) (semantic st1)) s
	| Repeat (b, st1)         -> Ccpo.fix (repeat_aux (eval_b_expr b) (semantic st1)) s
	| For    (x, a1, a2, st1) -> let b = eval_b_expr @@ L.Le(L.Var(x), a2) in
	                             let init = semantic @@ L.Assign(x, a1) in
	                             let inc = semantic @@ L.Assign(x, L.Sum(L.Var(x), L.Num(1))) in
	                             let body s0 = inc (semantic st1 s0) in
	                             Ccpo.fix (while_aux b body) @@ init @@ s
