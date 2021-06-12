%{

open AST

%}

%token AND "&"
%token OR "|"
%token LPAREN "(" RPAREN ")"
%token COLON ":"
%token <bool> INDENT
%token <string> TEXT
%token <string> COMMENT
%token EOF


%right OR
%right AND

%start <AST.element list> file

%%

let expression :=
  | t=TEXT;                   { Atom t }
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

let requirement :=
  | name=TEXT; ":"; name_comment=COMMENT?; n=INDENT;
    requirement = expression; comments = COMMENT*; n2=INDENT;
     { assert (n && not n2); { name; name_comment; requirement; comments } }

let element :=
  | ~=requirement;  { Req requirement }
  | c=COMMENT;      { Com c }

let file := l=element*; EOF; { l }

%%
