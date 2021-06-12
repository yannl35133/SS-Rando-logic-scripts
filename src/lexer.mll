{
open Location
open Parser

exception Syntax_Error of Location.location

let get_location lexbuf =
  let pos1 = Lexing.lexeme_start_p lexbuf
  and pos2 = Lexing.lexeme_end_p lexbuf in
  Loc (pos1, pos2)

let indent_size = 2

let get_indent =
  let cur = ref 0 in
  fun n ->
    if n = !cur then None
    else begin
      let already_indent = !cur >= indent_size in
      let diff = n > !cur in
      cur := n;
      if already_indent && n >= indent_size then
        None
      else Some (INDENT diff)
    end

}

let newline = '\n' | '\r' '\n'
let space = [' ' '\t']
let alphanum = ['A'-'Z' 'a'-'z' '_' '0'-'9' ''' '-']
let alpha = ['A'-'Z' 'a'-'z' '_']
let ascii = [' ' - '~']
let text = alphanum (ascii # ['(' ')' '&' '|' ':' '#'])*

rule token = parse
  | (newline+ as nl) (space* as indent)
      {
        for _ = 1 to String.length nl do Lexing.new_line lexbuf done;
        match (get_indent (String.length indent)) with Some v -> v | None -> token lexbuf
      }
  | space+          { token lexbuf }
  | "&"             { AND }
  | "|"             { OR }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | ":"             { COLON }
  | text as id      { TEXT (String.trim id) }
  | "#" ascii* as comment
                    { COMMENT comment }
  | eof             { EOF }
  | _
      { raise (Syntax_Error (get_location lexbuf)) }

{

}
