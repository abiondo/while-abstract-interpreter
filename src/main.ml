module L = Language
module S = Semantics
module VF = VariableFinder
open Utils

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

(* Builds a state with initialized variables for the given arguments *)
let rec argument_state (a : L.value list) : S.state =
	let rec build n l =
		match l with
		| [] -> S.State.empty
		| x :: xs -> S.State.add ("$" ^ (string_of_int n)) x (build (n+1) xs)
	in build 0 a

(* Fills in missing state variables with random values *)
let fill_state (vars : VF.VarSet.t) (s : S.state) : S.state =
	let f x s0 = if S.State.mem x s0 then s0 else S.State.add x (random_z ()) s0 in
	VF.VarSet.fold f vars s

(* Builds the initial state, given a program and a list of arguments *)
let initial_state (program : L.stm) (a : L.value list) : S.state =
	fill_state (VF.variables program) (argument_state a)

(* Prints a state to the standard output *)
let dump_state s =
	S.State.iter (fun k v -> Printf.printf "%6s -> %s\n" k (Z.to_string v)) s


let program = parse stdin
let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
let si = initial_state program @@ Array.to_list @@ Array.map Z.of_string args

let () =
	Random.self_init ();

	Printf.printf("---------------------\n");
	Printf.printf("    INITIAL STATE    \n");
	Printf.printf("---------------------\n");
	dump_state si;
	Printf.printf("---------------------\n\n");

	Printf.printf("Running...\n\n");
	flush stdout;

	let sf = S.semantic program si in

	Printf.printf("---------------------\n");
	Printf.printf("     FINAL STATE     \n");
	Printf.printf("---------------------\n");
	dump_state sf;
	Printf.printf("---------------------\n");
	flush stdout;
