open Format

(* two important global variables: [input_name] and [input_chan] *)
type location =
    Loc of (Lexing.position  (* Position of the first character *)
          * Lexing.position) (* Position of the next character following the last one *)

let print_location ff (Loc(p1,p2)) =
  let n1 = p1.pos_cnum - p1.pos_bol in (* character number *)
  let n2 = p2.pos_cnum - p2.pos_bol in
  let l1 = p1.pos_lnum in (* line number *)
  let l2 = p2.pos_lnum in
  let f1 = p1.pos_fname in (* file name *)
  let f2 = p2.pos_fname in

  if f1 != f2 then (* Strange case *)
    fprintf ff
    "File \"%s\" line %d, character %d, to file \"%s\" line %d, character %d@."
      f1 l1 n1 f2 l2 n2

  else if l2 > l1 then
    fprintf ff
      "File \"%s\", lines %d-%d, characters %d-%d:@." f1 l1 l2 n1 n2
  else
    fprintf ff "File \"%s\", line %d, characters %d-%d:@." f1 l1 n1 n2
