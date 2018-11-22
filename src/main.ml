module L = Language
module S = Semantics

let n = int_of_string Sys.argv.(1)

let program = L.Comp(
	L.Assign("x", L.Num(n)), L.Comp(
	L.Assign("y", L.Num(1)),
	L.While(L.Leq(L.Num(1), L.Var("x")), L.Comp(
		L.Assign("y", L.Mul(L.Var("x"), L.Var("y"))),
		L.Assign("x", L.Sub(L.Var("x"), L.Num(1)))))
	))

let s = S.semantic program S.State.empty

let get x = match x with
| None -> failwith "Undefined state"
| Some(v) -> v

let () = Printf.printf "%d! = %d\n" n (S.State.eval_var "y" (get s))
