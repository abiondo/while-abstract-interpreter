let parse (c : in_channel) : Language.stm =
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

module Domain : Domains.StateDomain = IntervalDomain

let initial_state =
	if Array.length Sys.argv < 2 then Domain.top
	else Domain.of_string Sys.argv.(1)

let program = parse stdin

let final_state = Domain.abstract_stm program initial_state

let () = Printf.printf "%s\n" (Domain.to_string final_state)
