{

open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }

}

let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

let num = '0' | ('-'? ['1'-'9'] ['0'-'9']*)
let ident = ['a'-'z' 'A'-'Z'] ['0'-'9' 'a'-'z' 'A'-'Z']*

rule token = parse
    white        { token lexbuf }
  | newline      { next_line lexbuf; token lexbuf }
  | "true"       { TRUE }
  | "false"      { FALSE }
  | ":="         { ASSIGN }
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }
  | "while"      { WHILE }
  | "do"         { DO }
  | num   as lxm { NUM(int_of_string lxm) }
  | ident as lxm { IDENT(lxm) }
  | '('          { LPAREN }
  | ')'          { RPAREN }
  | ';'          { SEMI }
  | '!'          { NOT }
  | '&'          { AND }
  | '='          { EQ }
  | "<="         { LE }
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | eof          { EOF }
  | _            { raise @@ SyntaxError ("Unexpected character: " ^ (lexeme lexbuf))}
