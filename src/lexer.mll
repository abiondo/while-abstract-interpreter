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

let white   = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

let digit    = ['0'-'9']
let digit_nz = ['1'-'9']
let alpha    = ['a'-'z' 'A'-'Z']
let alnum    = ['a'-'z' 'A'-'Z' '0'-'9']

let num_pos      = digit_nz digit*
let num_non_zero = '-'? num_pos
let num_non_neg  = '0' | num_pos
let num          = '0' | num_non_zero

let var   = alpha alnum*
let arg   = '$' num_non_neg
let ident = var | arg

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
