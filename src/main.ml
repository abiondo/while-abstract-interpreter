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

module Domain : Domains.Domain = IntervalDomain

let program = parse stdin

let final_state = Domain.StmSem.abstract_stm program Domain.StateLat.top

let () =
	Printf.printf "%s\n" (Domain.StateLat.to_string final_state)
