module L = Language
module S = Semantics

(* Parses a program *)
let parse (c : in_channel) : L.stm =
	let lexbuf = Lexing.from_channel c in
	try
		Parser.program Lexer.token lexbuf
	with exn ->
		begin
			let curr = lexbuf.Lexing.lex_curr_p in
			let line = curr.Lexing.pos_lnum in
			let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
			let tok = Lexing.lexeme lexbuf in
			Printf.eprintf
				"Parsing error: %s: line %d, column %d, token \"%s\"\n"
				(Printexc.to_string exn) line cnum tok;
			exit 1
		end

(* Builds the initial state given a list of arguments *)
let rec initial_state (a : L.value list) : S.state =
	let rec build n l =
		match l with
		| [] -> S.State.empty
		| x :: xs -> S.State.add ("$" ^ (string_of_int n)) x (build (n+1) xs)
	in build 0 a

(* Unwraps a state, failing if it is undefined *)
let get_state s =
	match s with
	| None      -> failwith "Undefined state"
	| Some (ss) -> ss


let program = parse stdin

let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
let s0 = initial_state @@ Array.to_list @@ Array.map int_of_string args

let sf = get_state @@ S.semantic program s0

let () = Printf.printf "y = %d\n" (S.State.eval_var "y" sf)
