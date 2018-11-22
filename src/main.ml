module S = Semantics

let program =
	let lexbuf = Lexing.from_channel stdin in
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

let x = int_of_string Sys.argv.(1)

let s = S.semantic program (S.State.add "x" x S.State.empty)

let get x = match x with
| None -> failwith "Undefined state"
| Some(v) -> v

let () = Printf.printf "y = %d\n" (S.State.eval_var "y" (get s))
