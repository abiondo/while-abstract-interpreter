(* The name of a variable *)
type var = string
(* The value of a variable *)
type value = Z.t

(* An arithmetic expression *)
type a_expr =
	| Num of value
	| Var of var
	| Sum of a_expr * a_expr
	| Sub of a_expr * a_expr
	| Mul of a_expr * a_expr

(* A boolean expression *)
type b_expr =
	| Bool of bool
	| And  of b_expr * b_expr
	| Or   of b_expr * b_expr
	| Eq   of a_expr * a_expr
	| Ne   of a_expr * a_expr
	| Le   of a_expr * a_expr
	| Ge   of a_expr * a_expr

(* A statement *)
type stm =
	| Skip
	| Assign of var * a_expr
	| Comp   of stm * stm
	| If     of b_expr * stm * stm
	| While  of b_expr * stm

(* Type of a folding function taking the current accumulator of type 'a and
 * an element of type 'b, and returning the new accumulator of type 'a. *)
type ('a, 'b) fold_func = 'a -> 'b -> 'a

(* Folds a_expr through f_a *)
let rec fold_a_expr (f_a : ('a, a_expr) fold_func) (acc : 'a) (a : a_expr) : 'a =
	let new_acc = f_a acc a in
	let g_a = fold_a_expr f_a in
	match a with
	| Num (_)
	| Var (_)      -> new_acc
	| Sum (a1, a2)
	| Sub (a1, a2)
	| Mul (a1, a2) -> g_a (g_a new_acc a1) a2

(* Folds b_expr through f_b, and f_a when a_expr is encountered *)
let rec fold_b_expr (f_a : ('a, a_expr) fold_func) (f_b : ('a, b_expr) fold_func)
                    (acc : 'a) (b : b_expr) : 'a =
	let new_acc = f_b acc b in
	let g_a = fold_a_expr f_a in
	let g_b = fold_b_expr f_a f_b in
	match b with
	| Bool (_)      -> new_acc
	| And  (b1, b2)
	| Or   (b1, b2) -> g_b (g_b new_acc b1) b2
	| Eq   (a1, a2)
	| Ne   (a1, a2)
	| Le   (a1, a2)
	| Ge   (a1, a2) -> g_a (g_a new_acc a1) a2

(* Folds stm through f_st, and f_a and f_b when a_expr and b_expr are encountered *)
let rec fold_stm (f_a : ('a, a_expr) fold_func) (f_b : ('a, b_expr) fold_func)
                 (f_st : ('a, stm) fold_func) (acc : 'a) (st : stm) : 'a =
	let new_acc = f_st acc st in
	let g_a = fold_a_expr f_a in
	let g_b = fold_b_expr f_a f_b in
	let g_st = fold_stm f_a f_b f_st in
	match st with
	| Skip                    -> new_acc
	| Assign (x, a)           -> g_a new_acc a
	| Comp   (st1, st2)       -> g_st (g_st new_acc st1) st2
	| If     (b, st1, st2)    -> g_st (g_st (g_b new_acc b) st1) st2
	| While  (b, st1)         -> g_st (g_b new_acc b) st1
