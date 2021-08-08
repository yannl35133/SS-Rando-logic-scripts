%{

open AST

%}

%token AND "&"
%token OR "|"
%token LPAREN "(" RPAREN ")"
%token <string> TEXT
%token EOF


%right OR
%right AND

%start <string AST.expression> full_expression

%%

let atom :=
  | t=TEXT;                   { Atom t }

let expression :=
  | ~=atom;                   { atom }
  | "("; ~=expression; ")";   { expression }
  | e1=expression; "&"; e2=expression; {
        match e2 with
        | Atom _ | Or _ -> And [e1; e2]
        | And l -> And (e1 :: l)
    }
  | e1=expression; "|"; e2=expression; {
        match e2 with
        | Atom _ | And _ -> Or [e1; e2]
        | Or l -> Or (e1 :: l)
    }

let full_expression := ~=expression; EOF; { expression }

%%
