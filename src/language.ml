(* The name of a variable *)
type var = string
(* The value of a variable *)
type value = int

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
	| Not  of b_expr
	| And  of b_expr * b_expr
	| Eq   of a_expr * a_expr
	| Leq  of a_expr * a_expr

(* A statement *)
type stm =
	| Skip
	| Assign of var * a_expr
	| Comp   of stm * stm
	| If     of b_expr * stm * stm
	| While  of b_expr * stm
