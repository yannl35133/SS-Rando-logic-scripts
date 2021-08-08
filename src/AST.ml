open Containers
module SMap = Map.Make (String)
module SSet = Set.Make (String)

exception Yaml_error

type allowed_time_of_day = DayOnly | NightOnly | Both

type atom = Item of string * int | Event of string

type 'atom expression =
  | Atom of 'atom
  | Or of 'atom expression list
  | And of 'atom expression list

type 'atom area = {
  name : string;
  allowed_time_of_day : allowed_time_of_day;
  can_sleep : bool;
  can_save : bool;
  sub_areas : 'atom area SMap.t;
  events : 'atom expression SMap.t;
  locations : 'atom expression SMap.t;
  gossip_stones : 'atom expression SMap.t;
  logical_exits : 'atom expression SMap.t;
  map_exits : 'atom expression SMap.t;
}

type 'atom areas = 'atom area SMap.t

let rec map_atom_exp f = function
  | Atom a -> Atom (f a)
  | And l -> And (List.map (map_atom_exp f) l)
  | Or l -> Or (List.map (map_atom_exp f) l)

let rec map_atom_area f area =
  {
    area with
    sub_areas = SMap.map (map_atom_area f) area.sub_areas;
    events = SMap.map (map_atom_exp f) area.events;
    locations = SMap.map (map_atom_exp f) area.locations;
    gossip_stones = SMap.map (map_atom_exp f) area.gossip_stones;
    logical_exits = SMap.map (map_atom_exp f) area.logical_exits;
    map_exits = SMap.map (map_atom_exp f) area.map_exits;
  }

let extractO : Yaml.value -> _ = function `O l -> l | _ -> raise Yaml_error

let extractA : Yaml.value -> _ = function `A l -> l | _ -> raise Yaml_error

let pp_space fmt _ = Fmt.string fmt " "

let pp_tod fmt = function
  | Both -> Fmt.string fmt "Both"
  | DayOnly -> Fmt.string fmt "DayOnly"
  | NightOnly -> Fmt.string fmt "NightOnly"

let rec pp_expr fmt = function
  | Atom s -> Format.fprintf fmt "%s" s
  | Or lst ->
      Format.fprintf fmt "%a" Fmt.(list ~sep:(unit " |@ ") pp_expr_atom) lst
  | And lst ->
      Format.fprintf fmt "%a" Fmt.(list ~sep:(unit " &@ ") pp_expr_atom) lst

and pp_expr_atom fmt = function
  | Atom _ as e -> pp_expr fmt e
  | (Or _ | And _) as e -> Format.fprintf fmt "(@[<hv2>@,%a@,@])" pp_expr e

let pp_named_exp_list category fmt smap =
  if not @@ SMap.is_empty smap then
    Format.fprintf fmt "@[<hv2>%s:@ %a@]" category
      Fmt.(
        list ~sep:(unit "@,") (fun fmt (n, e) ->
            pf fmt "@[<hv2>%s:@ %a@]" n pp_expr e))
      (SMap.to_list smap)

let rec pp_area fmt
    {
      name;
      allowed_time_of_day;
      can_sleep;
      can_save;
      sub_areas;
      events;
      locations;
      gossip_stones;
      logical_exits;
      map_exits;
    } =
  Format.fprintf fmt
    "@[<v2>%s:@,\
     allowed-time-of-day: %a@,\
     can-sleep: %b@,\
     can-save: %b@,\
     %a@,\
     %a@,\
     %a@,\
     %a@,\
     %a@,\
     %a@,\
     @]"
    name pp_tod allowed_time_of_day can_sleep can_save
    (pp_named_exp_list "events")
    events
    (pp_named_exp_list "locations")
    locations
    (pp_named_exp_list "gossip-stones")
    gossip_stones
    (pp_named_exp_list "logical-exits")
    logical_exits
    (pp_named_exp_list "map_exits")
    map_exits
    Fmt.(
      list ~sep:(unit "@,") (fun fmt (k, e) ->
          pf fmt "@[<hv2>%s:@ %a@]" k pp_area e))
    (SMap.to_list sub_areas)
