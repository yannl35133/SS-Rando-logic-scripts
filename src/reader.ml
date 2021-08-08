open Containers
open AST

exception Yaml_error

let get_bool : Yaml.value -> bool = function
  | `Bool b -> b
  | _ -> raise Yaml_error

let get_tod : Yaml.value -> allowed_time_of_day = function
  | `String "DayOnly" -> DayOnly
  | `String "NightOnly" -> NightOnly
  | `String "Both" -> Both
  | _ -> raise Yaml_error

let extract_field (field : string) (unyaml : Yaml.value -> 'a) ~(default : 'a) l
    =
  l
  |> List.Assoc.get ~eq:String.( = ) field
  |> Option.map unyaml |> Option.value ~default

let extract_capitalized_fields (unyaml : string -> Yaml.value -> 'a) l =
  l
  |> List.filter (fun (s, _) -> String.(capitalize_ascii s = s))
  |> List.map (fun (name, v) -> (name, unyaml name v))
  |> SMap.of_list

let get_expression : Yaml.value -> _ expression = function
  | `String s ->
      MenhirLib.Convert.Simplified.traditional2revised Parser.full_expression
        (Lexer.token (Sedlexing.Utf8.from_string s))
  | _ -> raise Yaml_error

let get_expression_list : Yaml.value -> _ expression SMap.t =
 fun v ->
  v |> extractO
  |> List.map (fun (name, v) -> (name, get_expression v))
  |> SMap.of_list

let construct_world : (string * Yaml.value) list -> _ areas * _ =
  let entrances = ref [] in
  let rec construct_area rev_prefix name ~allowed_tod v =
    v |> extractO |> fun fields ->
    let rev_prefix = name :: rev_prefix in
    let allowed_tod =
      extract_field "allowed-time-of-day" get_tod ~default:allowed_tod fields
    in
    let logical_entrances =
      extract_field "logical-entrances" get_expression_list ~default:SMap.empty
        fields
    in
    entrances := (rev_prefix, logical_entrances) :: !entrances;
    {
      name;
      allowed_time_of_day = allowed_tod;
      can_sleep = extract_field "can-sleep" get_bool ~default:false fields;
      can_save = extract_field "can-sleep" get_bool ~default:false fields;
      sub_areas =
        extract_capitalized_fields
          (construct_area rev_prefix ~allowed_tod)
          fields;
      events =
        extract_field "events" get_expression_list ~default:SMap.empty fields;
      locations =
        extract_field "locations" get_expression_list ~default:SMap.empty fields;
      gossip_stones =
        extract_field "gossip-stones" get_expression_list ~default:SMap.empty
          fields;
      logical_exits =
        extract_field "logical-exits" get_expression_list ~default:SMap.empty
          fields;
      map_exits =
        extract_field "map-exits" get_expression_list ~default:SMap.empty fields;
    }
  in
  fun yaml ->
    let res =
      extract_capitalized_fields (construct_area [] ~allowed_tod:DayOnly) yaml
    in
    (res, !entrances)

let pre_logic, entrances =
  Yaml_unix.of_file_exn Fpath.(v "logic.yaml") |> extractO |> construct_world

(* Interpreting atoms *)
let rec subname short full =
  match (short, full) with
  | [ name1 ], [ name2 ] -> String.(name1 = name2)
  | [], _ | _, [] -> false
  | hd1 :: tl1, hd2 :: tl2 when String.(hd1 = hd2) -> subname tl1 tl2
  | l1, _ :: l2 -> subname l1 l2

let find_short full =
  SSet.find_first (fun short -> subname (String.split ~by:" - " short) full)

let rec get_assoc_list rev_prefix
    {
      name;
      sub_areas;
      events;
      locations;
      gossip_stones;
      logical_exits = _;
      map_exits;
      _;
    } =
  let rev_prefix = name :: rev_prefix in
  let get_pairs data lst =
    List.map
      (fun (name, _) ->
        List.(rev (name :: rev_prefix)) |> fun full ->
        (find_short full data, full))
      lst
  in
  let event_pairs = get_pairs Data.events (SMap.to_list events) in
  let locations_pairs =
    get_pairs
      (SSet.of_seq @@ Seq.map fst @@ SMap.to_seq Data.checks)
      (SMap.to_list locations)
  in
  let gossip_pairs =
    get_pairs Data.gossip_stones (SMap.to_list gossip_stones)
  in
  let map_pairs = get_pairs Data.map_exits (SMap.to_list map_exits) in
  event_pairs @ locations_pairs @ gossip_pairs @ map_pairs
  @ List.concat_map
      (get_assoc_list rev_prefix)
      (List.map snd @@ SMap.to_list sub_areas)

let short_full_assoc_list : (string, string list) List.Assoc.t =
  List.concat_map (get_assoc_list []) (List.map snd @@ SMap.to_list pre_logic)

let rec search_area src dst areas =
  match dst with
  | [] -> Some []
  | hd :: tl -> (
      match SMap.find_opt hd areas with
      | Some area ->
          search_area [] tl area.sub_areas
          |> Option.map (fun suffix -> hd :: suffix)
      | None -> (
          match src with
          | hd' :: tl' ->
              areas |> SMap.find hd' |> fun area ->
              search_area tl' dst area.sub_areas
              |> Option.map (fun suffix -> hd' :: suffix)
          | [] ->
              areas
              |> SMap.filter_map (fun _ area ->
                     search_area [] dst area.sub_areas)
              |> SMap.choose_opt |> Option.map snd))

let rec update_full_area_names rev_prefix
    ({ name; logical_exits; sub_areas; _ } as area) =
  let rev_prefix = name :: rev_prefix in
  {
    area with
    logical_exits =
      logical_exits |> SMap.bindings
      |> List.map (fun (name, v) ->
             name |> String.split ~by:" - " |> fun dst ->
             search_area (List.rev rev_prefix) dst pre_logic
             |> Option.get_exn_or "Unknown relative area"
             |> String.concat "/"
             |> fun name -> (name, v))
      |> SMap.of_list;
    sub_areas = SMap.map (update_full_area_names rev_prefix) sub_areas;
  }

let rec edit_at addr f areas =
  match addr with
  | [] -> invalid_arg "Empty address"
  | [ a ] ->
      SMap.update a
        (function
          | None -> invalid_arg "Unknown address" | Some area -> Some (f area))
        areas
  | a :: tl ->
      SMap.update a
        (function
          | None -> invalid_arg "Unknown address"
          | Some area ->
              Some { area with sub_areas = edit_at tl f area.sub_areas })
        areas

let add_entrances logic =
  List.fold_left
    (fun logic (base, entrances) ->
      SMap.fold
        (fun dst exp logic ->
          let area_name =
            search_area base (String.split ~by:" - " dst) logic
            |> Option.get_exn_or "Unknown relative area"
          in
          edit_at area_name
            (fun area ->
              {
                area with
                logical_exits =
                  SMap.add (String.concat "/" base) exp area.logical_exits;
              })
            logic)
        entrances logic)
    logic entrances

let edit_atom s =
  if Str.(string_match (regexp {|\([^×]+\)×\(\d+\)|}) s 0) then (
    let name = Str.matched_group 1 s
    and number = int_of_string (Str.matched_group 2 s) in
    assert (SSet.mem name Data.items);
    Item (name, number))
  else if SSet.mem s Data.items then Item (s, 1)
  else (
    assert (SSet.mem s Data.events);
    Event s)

let logic =
  pre_logic
  |> SMap.map (update_full_area_names [])
  |> add_entrances
  |> SMap.map (map_atom_area edit_atom)
