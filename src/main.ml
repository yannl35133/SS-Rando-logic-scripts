open AST
let rando_path = Sys.getenv "sslib_dir"
let requirements_path = Filename.concat rando_path "SS Rando Logic - Requirements.yaml"
let glitchless_path   = Filename.concat rando_path "SS Rando Logic - Glitchless Requirements.yaml"
let glitched_path     = Filename.concat rando_path "SS Rando Logic - Glitched Requirements.yaml"

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let init f =
  f
  |> List.filter_map (function | Req r -> Some r | Com _ -> None)
  |> List.map (fun r -> (r.name, r.requirement))
  |> List.to_seq
  |> StringMap.of_seq

module Op = struct
  type able = ALWAYS | SOMETIMES | NEVER

  let (||) a b = match (a, b) with
    | ALWAYS, _ | _, ALWAYS -> ALWAYS
    | SOMETIMES, _ | _, SOMETIMES -> SOMETIMES
    | NEVER, NEVER -> NEVER

  let (&&) a b = match (a, b) with
  | NEVER, _ | _, NEVER -> NEVER
  | SOMETIMES, _ | _, SOMETIMES -> SOMETIMES
  | ALWAYS, ALWAYS -> ALWAYS
end
open Op

let rec eval atom = function
  | Atom "Impossible" -> NEVER
  | Atom "Nothing"    -> ALWAYS
  | Atom req          -> atom req
  | Or  l -> List.fold_left (||) NEVER  @@ List.map (eval atom) l
  | And l -> List.fold_left (&&) ALWAYS @@ List.map (eval atom) l

let ignore_first () = Fun.id

let eval_nocache cache map =
  let seen = ref StringSet.empty in
  let rec atom req =
    match StringSet.mem req !seen with
    | true -> NEVER
    | false -> begin match StringMap.find req !cache with
      | Some able -> able
      | None | (exception Not_found) ->
        req
        |> ignore_first (seen := StringSet.add req !seen)
        |> Fun.flip StringMap.find_opt map
        |> Option.fold ~none:SOMETIMES ~some:(eval atom)
        |> ignore_first (seen := StringSet.remove req !seen)
    end
  in eval atom

let eval map =
  let cache = ref StringMap.empty in
  let rec atom req =
    match StringMap.find req !cache with
    | Some able -> able
    | None -> eval_nocache cache map (Atom req)
    | (exception Not_found) ->
        req
        |> ignore_first (cache := StringMap.add req None !cache)
        |> Fun.flip StringMap.find_opt map
        |> Option.fold ~none:SOMETIMES ~some:(eval atom)
        |> fun b -> cache := StringMap.add req (Some b) !cache; b
  in eval atom


let rec flatten_expr = function
  | Atom r -> Atom r
  | Or [] -> Atom "Impossible"
  | Or (hd :: tl) -> begin match flatten_expr hd with
    | Atom "Nothing" -> Atom "Nothing"
    | Atom "Impossible" -> flatten_expr @@ Or tl
    | Or l1 -> flatten_expr @@ Or (l1 @ tl)
    | (And _ | Atom _) as r -> begin match flatten_expr (Or tl) with
      | Atom "Nothing" -> Atom "Nothing"
      | Or l -> Or (r :: l)
      | Atom "Impossible" -> r
      | s -> Or ([r; s])
    end
  end
  | And [] -> Atom "Nothing"
  | And (hd :: tl) -> begin match flatten_expr hd with
    | Atom "Impossible" -> Atom "Impossible"
    | Atom "Nothing" -> flatten_expr @@ And tl
    | And l1 -> flatten_expr @@ And (l1 @ tl)
    | (Or _ | Atom _) as r -> begin match flatten_expr (And tl) with
      | Atom "Impossible" -> Atom "Impossible"
      | And l -> And (r :: l)
      | Atom "Nothing" -> r
      | s -> And ([r; s])
    end
  end


let prune map file =
  let eval_map = eval map in
  let rec prune_exp = function
    | Atom r -> begin match eval_map (Atom r) with
      | ALWAYS -> Atom "Nothing"
      | SOMETIMES -> Atom r
      | NEVER -> Atom "Impossible"
    end
    | Or  l -> Or  (List.map prune_exp l)
    | And l -> And (List.map prune_exp l)
  in
  let prune_requirement r =
    { r with requirement = r.name
      |> Fun.flip StringMap.find map
      |> prune_exp
      |> flatten_expr
    }
  in
  file
  |> List.map (function Com c -> Com c | Req r -> Req (prune_requirement r))
  |> List.filter (function Com _ -> true | Req { requirement = Atom "Impossible"; _} -> false | Req _ -> true)


let make_impossible name =
  StringMap.add name @@ Atom "Impossible"

let make_trivial name =
  StringMap.add name @@ Atom "Nothing"

let glitchless_tricks = [
  "Upgrade Hook Beetle";
  "Waterfall Cave Jump";
  "Baby Rattle from Beedle's Shop";
  "Sky Keep Entrance Jump";
  "Sky - Volcanic Island Dive";
  "Sky - Beedle's Island Cage Chest Dive";
  "Thunderhead - East Island Dive";
  "Gym's Rope Jump";
  "Early Lake Floria - Fence Hop";
  "Early Lake Floria - Swordless Rope Floria";
  "Faron - Bokoblin Luring";
  "Digging Mitts Fight with Hook Beetle";
  "Itemless First Timeshift Stone";
  "Lanayru Mines Brakeslide";
  "Lanayru Desert - Brakeslide to Sand Oasis";
  "Lanayru Desert - Spume Brakeslide";
  "Lanayru Desert - Sand Oasis Brakeslide";
  "Temple of Time - Slingshot Shot";
  "Temple of Time - Bomb Throw";
  "Temple of Time Skip - Brakeslide";
  "Secret Passageway Hook Beetle Opening";
  "Lightning Node End with Bombs";
  "Fire Node - Brakeslide";
  "Cactus Bomb Whip";
  "Skipper's Retreat Fast Clawshots";
  "Skyview - Spider Roll";
  "Skyview Slingshot Shot";
  "Earth Temple - Keese Yeet";
  "Earth Temple - Slope Stuttersprint";
  "Earth Temple - Bomb Flower Scaldera";
  "LMF - Whip First Room Switch";
  "LMF - Key Locked Room Bomb Throw";
  "LMF - Keylocked Slingshot Trickshot";
  "LMF - Minecart Jump";
  "LMF - Molderach without Gust Bellows";
  "Ancient Cistern - Cistern Clip";
  "Ancient Cistern - Cistern Whip Room Clip";
  "Ancient Cistern - Basement Highflip";
  "Ancient Cistern - Map Chest Jump";
  "Ancient Cistern - Lever Jump";
  "Ancient Cistern - Basement Lilypad Clip";
  "Sandship - No Combination Hint";
  "Sandship - Itemless Spume Skip";
  "Sandship - Mast Jump";
  "Fire Sanctuary - Pillar Jump";
  "Fire Sanctuary - Swordless Pillar Jump";
  "Fire Sanctuary - No Bombable Wall Hint";
  "Sky Keep - Shooting LMF Bow Switches in Present";
  "Sky Keep - FS Room Clawshots Vine Clip";
] |> List.rev_map (fun s -> s ^ " Trick")


let glitched_tricks = [
  "Bed Trick";
  "Owlan Crystals without Bombs";
  "Ancient Cistern - Lilypad Skip";
  "Ancient Cistern - Swordless Cistern Clip";
] |> List.rev_map (fun s -> s ^ " Trick")

let () =
  let f = open_in requirements_path in
  let buf = Lexing.from_channel f in

  let file =
    try
      Parser.file Lexer.token buf
    with
    | Lexer.Syntax_Error e -> Format.eprintf "%a" Location.print_location e; exit 1
  in
  close_in f;

  let req_map = init file in
  let glitched_map = req_map
    |> fun map -> List.fold_left (Fun.flip make_trivial) map glitchless_tricks
    |> make_trivial "BiT Trick" |> make_trivial "Early Thunderhead Trick"
  in

  let glitched_file = prune glitched_map file in
  let glitched_output = Format.formatter_of_out_channel @@ open_out glitched_path in
  Format.pp_set_margin glitched_output 100;
  Format.fprintf glitched_output "%a@." pp_file glitched_file;

  let can_set_scene_flags =
    List.filter_map (function Req r when Str.string_match (Str.regexp {|Can Set Scene Flag .x.*|}) r.name 0 -> Some r.name | _ -> None) file
  in
  let glitchless_map = req_map
    |> make_impossible "BiT Trick"
    |> fun map -> List.fold_left (Fun.flip make_impossible) map glitched_tricks
    |> fun map -> List.fold_left (Fun.flip make_impossible) map can_set_scene_flags

  in
  let glitchless_file = prune glitchless_map file in
  let glitchless_file = List.filter (function Com c -> not @@ Str.string_match (Str.regexp {|# Can RBM .x.*|}) c 0 | Req _ -> true) glitchless_file in
  let glitchless_output = Format.formatter_of_out_channel @@ open_out glitchless_path in
  Format.pp_set_margin glitchless_output 100;
  Format.fprintf glitchless_output "%a@." pp_file glitchless_file;
  ()
