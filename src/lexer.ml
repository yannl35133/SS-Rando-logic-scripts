open Location
open Parser

exception Syntax_Error of Location.location

let newline = [%sedlex.regexp? '\n' | "\r\n"]

let space = [%sedlex.regexp? ' ' | '\t']

let alphanum =
  [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' | '\'' | '-']

let alpha = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z' | '_']

let ascii = [%sedlex.regexp? ' ' .. '~']

let text =
  [%sedlex.regexp? alphanum, Star (Sub (ascii, ('(' | ')' | '&' | '|')))]

let rec token lexbuf =
  match%sedlex lexbuf with
  | newline ->
      Sedlexing.new_line lexbuf;
      token lexbuf
  | Plus space -> token lexbuf
  | '&' -> AND
  | '|' -> OR
  | '(' -> LPAREN
  | ')' -> RPAREN
  | text -> TEXT (String.trim (Sedlexing.Utf8.lexeme lexbuf))
  | eof -> EOF
  | _ -> raise (Syntax_Error (Loc (Sedlexing.lexing_positions lexbuf)))

let token = Sedlexing.with_tokenizer token
