type expression =
  | Atom of string
  | Or of expression list
  | And of expression list

type requirement = {
  name: string;
  name_comment: string option;
  requirement: expression;
  comments: string list;
}

type element =
  | Req of requirement
  | Com of string

let pp_space fmt _ = Fmt.string fmt " "

let rec pp_expr fmt = function
  | Atom s ->  Format.fprintf fmt "%s" s
  | Or lst ->  Format.fprintf fmt "%a" (Fmt.list ~sep:(fun fmt () -> Format.fprintf fmt " |@ ") pp_expr_atom) lst
  | And lst -> Format.fprintf fmt "%a" (Fmt.list ~sep:(fun fmt () -> Format.fprintf fmt " &@ ") pp_expr_atom) lst
and pp_expr_atom fmt = function
  | Atom _         as e -> Format.fprintf fmt "%a" pp_expr e
  | (Or _ | And _) as e -> Format.fprintf fmt "@[<hv 2>(@,%a@;<0 -2>)@]" pp_expr e

let pp_requirement fmt req =
  Format.fprintf fmt "@[<v>%a:%a@,  @[<v>@[<hv>%a@]%a@]@]@."
    Fmt.string req.name
    Fmt.(option @@ pp_space ++ string) req.name_comment
    pp_expr req.requirement
    Fmt.(list ~sep:nop @@ cut ++ string) req.comments

let pp_element fmt = function
  | Req r -> pp_requirement fmt r
  | Com c -> Format.fprintf fmt "%s@." c

let pp_file =
  Fmt.list pp_element
