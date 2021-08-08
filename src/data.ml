open Containers
open AST

(* Checks *)

type check_type = |

let check_type_of_string : string -> check_type = function
  | _ -> raise Yaml_error

let get_check_types : Yaml.value -> _ = function
  | `O lst -> (
      match List.assoc_opt ~eq:String.( = ) "type" lst with
      | Some (`String s) ->
          s |> String.split ~by:", " |> List.map check_type_of_string
      | _ -> raise Yaml_error)
  | _ -> raise Yaml_error

let checks =
  Yaml_unix.of_file_exn Fpath.(v "checks.yaml")
  |> extractO
  |> List.map (fun (name, value) -> (name, get_check_types value))
  |> SMap.of_list

(* Gossip stones *)
let gossip_stones =
  Yaml_unix.of_file_exn Fpath.(v "hints.yaml")
  |> extractO |> List.map fst |> SSet.of_list

(* Events *)
let events =
  Yaml_unix.of_file_exn Fpath.(v "events.yaml")
  |> extractO |> List.map fst |> SSet.of_list

(* Map exits *)
let map_exits =
  Yaml_unix.of_file_exn Fpath.(v "exits.yaml")
  |> extractO |> List.map fst |> SSet.of_list

(* Items *)
let items =
  Yaml_unix.of_file_exn Fpath.(v "items.yaml")
  |> extractA
  |> List.map (function
       | `O lst -> List.assoc ~eq:String.( = ) "name" lst
       | _ -> raise Yaml_error)
  |> List.map (function `String s -> s | _ -> raise Yaml_error)
  |> SSet.of_list
