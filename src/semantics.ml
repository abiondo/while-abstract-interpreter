module L = Language
open Utils

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
type sem = state -> state
(* Type for partial semantic functions *)
type sem_partial = state -> state option

(* Evaluates an arithmetic expression *)
let rec eval_a_expr (a : L.a_expr) (s : state) : L.value =
	(* Table 1.1 *)
	match a with
	| Num (n)      -> n
	| Var (x)      -> State.eval_var x s
	| Sum (a1, a2) -> Z.add (eval_a_expr a1 s) (eval_a_expr a2 s)
	| Sub (a1, a2) -> Z.sub (eval_a_expr a1 s) (eval_a_expr a2 s)
	| Mul (a1, a2) -> Z.mul (eval_a_expr a1 s) (eval_a_expr a2 s)

(* Evaluates a boolean expression *)
let rec eval_b_expr (b : L.b_expr) (s : state) : bool =
	(* Table 1.2 (extended) *)
	match b with
	| Bool (b1)     -> b1
	| Not  (b1)     -> not (eval_b_expr b1 s)
	| And  (b1, b2) -> (eval_b_expr b1 s) && (eval_b_expr b2 s)
	| Eq   (a1, a2) -> (eval_a_expr a1 s) == (eval_a_expr a2 s)
	| Le   (a1, a2) -> (eval_a_expr a1 s) <= (eval_a_expr a2 s)

(* Auxiliary function whose fixpoint is the while semantic function *)
let while_aux (b : state -> bool) (sm : sem) (g : sem_partial) : sem_partial =
	cond b (g %. partial sm) (partial id)

(* Semantic function for a statement *)
let rec semantic (st : L.stm) (s : state) : state =
	(* Table 4.1 (extended) *)
	match st with
	| Skip                    -> s
	| Assign (x, a)           -> State.add x (eval_a_expr a s) s
	| Comp   (st1, st2)       -> semantic st2 @@ semantic st1 s
	| If     (b, st1, st2)    -> cond (eval_b_expr b) (semantic st1) (semantic st2) s
	| While  (b, st1)         -> Ccpo.fix (while_aux (eval_b_expr b) (semantic st1)) s
