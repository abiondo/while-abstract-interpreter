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
	(* Lemma 4.25: graph of lub = union of graphs of elements,
	 * i.e., lub fs x = y iff f x = y for some f in fs. *)
	match fs with
	| Cons (f, tail) ->
		match f x with
		| Some (y) -> y
		| None     -> lub (tail ()) x

(* Stream of powers of a total D->D function *)
let rec func_powers f x =
	Cons (x, fun () -> func_powers f (f x))

(* Least fixed point of a continuous D->D function *)
let fix (f : ('a, 'b) elm -> ('a, 'b) elm) (x : 'a) : 'b =
	(* Theorem 4.37: Kleene-Knaster-Tarski fixpoint iteration,
	 * i.e., fix f = lub {f^n bottom | n in N} *)
	lub (func_powers f bottom) x
