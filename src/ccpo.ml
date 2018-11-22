(*
A chain-complete partial order (D, <=):
  - D = set of (partial) functions X->X
  - f1 <= f2 iff f1(x) = y => f2(x) = y for all x, y in X
*)

type 'a element = 'a -> 'a option

type 'a stream = Cons of 'a * (unit -> 'a stream)

(* Least element, i.e., f(x) = undef *)
let bottom x = None

(* Least upper bound of a D stream *)
let rec lub (fs : 'a element stream) (x : 'a) : 'a =
	match fs with
	| Cons (f, fst) ->
		match f x with
		| Some (y) -> y
		| None     -> lub (fst ()) x

(* Stream of powers of a total D->D function *)
let rec func_powers f x =
	Cons (x, fun () -> func_powers f (f x))

(* Kleene-Knaster-Tarski fixed point for a continuous D->D function *)
let fix (f : 'a element -> 'a element) (x : 'a) : 'a =
	lub (func_powers f bottom) x
