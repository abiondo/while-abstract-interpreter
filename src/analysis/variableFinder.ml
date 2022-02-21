module L = Language

(* Set of variable identifiers *)
module VarSet = Set.Make(struct
	type t = L.var
	let compare = compare
end)

(* Collects variables from L.a_expr *)
let collect_a_expr (vars : VarSet.t) (a : L.a_expr) : VarSet.t =
	match a with
	| Var (x) -> VarSet.add x vars
	| _       -> vars

(* Collects variables from L.b_expr *)
let collect_b_expr (vars : VarSet.t) (a : L.b_expr) : VarSet.t =
	vars

(* Collects variables from L.stm *)
let collect_stm (vars : VarSet.t) (st : L.stm) : VarSet.t =
	match st with
	| Assign (x, _)
	| For    (x, _, _, _) -> VarSet.add x vars
	| _                   -> vars

(* Returns all the variables defined or used in st *)
let variables (st : L.stm) : VarSet.t =
	L.fold_stm collect_a_expr collect_b_expr collect_stm VarSet.empty st
