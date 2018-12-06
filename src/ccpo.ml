(*
A chain-complete partial order (D, <=):
  - D = set of (partial) functions X->Y
  - f1 <= f2 iff f1(x) = y => f2(x) = y for all x in X, y in Y
*)

(* An element of the CCPO *)
type ('a, 'b) elm = 'a -> 'b option

(* An infinite sequence of elements *)
type ('a, 'b) seq =
	| Cons of ('a, 'b) elm * (unit -> ('a, 'b) seq)

(* Least element, i.e., f(x) = undef *)
let bottom x = None

(* Least upper bound of a sequence of elements *)
let rec lub (fs : ('a, 'b) seq) (x : 'a) : 'b =
	match fs with
	| Cons (f, fst) ->
		match f x with
		| Some (y) -> y
		| None     -> lub (fst ()) x

(* Stream of powers of a total D->D function *)
let rec func_powers f x =
	Cons (x, fun () -> func_powers f (f x))

(* Kleene-Knaster-Tarski fixed point for a continuous D->D function *)
let fix (f : ('a, 'b) elm -> ('a, 'b) elm) (x : 'a) : 'b =
	lub (func_powers f bottom) x
