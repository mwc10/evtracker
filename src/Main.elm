module Main exposing (..)
import Browser
import Dict exposing (Dict)
import Html exposing (div, select, option, text, p, span, label, h1, h2, input, button, form, li, ul)
import Html.Events exposing (onInput, onClick, onSubmit)
import Html.Attributes as A

-- TODOs
--  * Support more than one type of EV yield per pokemon

-- Main --
main =
  Browser.sandbox { init = init, update = update, view = view }

-- Model --
type alias Model = 
  { item : Maybe Stat
  , pokerus : Bool
  , targetEvs : StatSet
  , earnedEvs : StatSet
  , addPkmn : Maybe NewPkmn
  , uid : Int
  , pkmnList : List PKMN
  }

type alias StatSet =
  { hp  : Int
  , att : Int
  , def : Int
  , spa : Int
  , spd : Int
  , spe : Int
  }

type Stat = HP | Att | Def | SpA | SpD | Spe

type alias PKMN = 
  { id : Int
  , yield : Int
  , name : Maybe String
  , stat : Stat
  , count : Int
  , status : PkmnStatus
  }
-- TODO: model multiple stat ev yield pokmeon (up to three stats)
type alias NewPkmn =
  { name : Maybe String
  , stat : Stat
  , yield : Int
  }

type PkmnStatus
  = SetPkmn
  | UpdatePkmn

init : Model
init = 
  Model Nothing False zero_statset zero_statset Nothing 0 []

-- Update --
type Msg
  = ChangeItem String
  | TogglePokerus String
  | SetEvTarget Stat String
  | EvBerry Stat
  | ResetEarnedEvs
  | ResetTargetEvs
  | StartAddingNewPkmn
  | NewPkmnUpdate NewPkmnMsg
  | CancelAddingNewPkmn
  | AddNewPkmnToList
  | KilledPkmn Int
  | RemovePkmn Int

type NewPkmnMsg
  = NewPkmnName String
  | NewPkmnStat String
  | NewPkmnYield String

update : Msg -> Model -> Model
update msg model = 
  case msg of
    ChangeItem item ->
      { model | item = (item_str_to_stat item) }
    TogglePokerus _ ->
      { model | pokerus = not model.pokerus }
    SetEvTarget stat val ->
      { model | targetEvs = (update_target_ev model.targetEvs stat val) }
    EvBerry stat ->
      { model | earnedEvs = apply_ev_berry model stat }
    ResetEarnedEvs ->
      { model | earnedEvs = zero_statset, pkmnList = reset_all_kos model.pkmnList }
    ResetTargetEvs ->
      { model | targetEvs = zero_statset }
    StartAddingNewPkmn -> 
      { model | addPkmn = Just (NewPkmn Nothing HP 1) }
    CancelAddingNewPkmn -> 
      { model | addPkmn = Nothing }
    NewPkmnUpdate newMsg ->
      { model | addPkmn = update_new_pkmn newMsg model.addPkmn }
    AddNewPkmnToList -> 
      add_new_pkmn model
    KilledPkmn id -> 
      add_new_ko model id
    RemovePkmn id ->
      {model | pkmnList = remove_pkmn model.pkmnList id}


apply_ev_berry : Model -> Stat -> StatSet
apply_ev_berry model stat =
  let
    oldEv = get_stat_value model.earnedEvs stat
    newEv = if oldEv > 100 then 100 else oldEv - 10
      |> clamp_ev
  in
  set_setstat_val model.earnedEvs stat newEv

remove_pkmn : List PKMN -> Int -> List PKMN
remove_pkmn list id =
  List.filter (\p -> p.id /= id) list

reset_all_kos : List PKMN -> List PKMN
reset_all_kos pkmns =
  List.map (\p -> {p | count = 0}) pkmns

add_new_ko : Model -> Int -> Model
add_new_ko model id =
  let
    add_koes_with_model = add_koed_evs model

    updatedEarned = model.pkmnList
      |> List.filter (\p -> p.id == id)
      |> List.foldl add_koes_with_model model.earnedEvs

    updatedPkmnList = model.pkmnList
      |> List.map (add_pkmn_ko_count id)
  in
  {model | pkmnList = updatedPkmnList, earnedEvs = updatedEarned}

add_pkmn_ko_count : Int -> PKMN -> PKMN
add_pkmn_ko_count id pkmn =
  if pkmn.id == id then
    { pkmn | count = 1 + pkmn.count }
  else 
    pkmn

add_koed_evs : Model -> PKMN -> StatSet -> StatSet
add_koed_evs model pkmn acc =
  let
    heldYield = model.item
      |> Maybe.map (\stat -> (stat, 8))
    
    pokerusMutl = if model.pokerus then 2 else 1
    
    statList = (case heldYield of
        Just held -> [held, (pkmn.stat, pkmn.yield)]
        Nothing -> [(pkmn.stat, pkmn.yield)]
      )
      |> List.map (\(s, y) -> (s, y * pokerusMutl))
  in
    List.foldl add_val_to_statset acc statList

add_val_to_statset : (Stat, Int) -> StatSet -> StatSet 
add_val_to_statset (stat, n) set =
  let
    clampedN = max_possible_ev set n
  in
  case stat of 
    HP -> { set | hp = clamp_ev <| clampedN + set.hp }
    Att -> { set | att = clamp_ev <| clampedN + set.att }
    Def -> { set | def = clamp_ev <| clampedN + set.def }
    SpA -> { set | spa = clamp_ev <| clampedN + set.spa }
    SpD -> { set | spd = clamp_ev <| clampedN + set.spd }
    Spe -> { set | spe = clamp_ev <| clampedN + set.spe }

add_new_pkmn : Model -> Model
add_new_pkmn model =
  let
    uid = model.uid + 1
    make_new_list pkmn = 
      model.pkmnList
        |> List.foldl (::) []
        |> (::) (newpkmn_to_pkmn pkmn uid)
        |> List.foldl (::) []
  in
  case model.addPkmn of
    Just pkmn ->
      { model 
      | addPkmn = Nothing
      , uid = uid
      , pkmnList = make_new_list pkmn 
      }
    Nothing -> model

newpkmn_to_pkmn : NewPkmn -> Int -> PKMN
newpkmn_to_pkmn new id  =
  PKMN id new.yield new.name new.stat 0 SetPkmn

update_new_pkmn : NewPkmnMsg -> Maybe NewPkmn -> Maybe NewPkmn
update_new_pkmn msg pkmn =
  case msg of
    NewPkmnName name -> 
      Maybe.map (update_newpkmn_name name) pkmn
    NewPkmnStat statstr -> 
      Maybe.map (update_newpkmn_stat statstr) pkmn
    NewPkmnYield yieldstr ->
      Maybe.map (update_newpkmn_yield yieldstr) pkmn 

update_newpkmn_name : String -> NewPkmn -> NewPkmn 
update_newpkmn_name name pkmn = { pkmn | name = (Just name)}

update_newpkmn_stat : String -> NewPkmn -> NewPkmn 
update_newpkmn_stat statstr pkmn = 
  let
    stat = statstr
      |> str_to_stat
      |> Maybe.withDefault HP
  in
  {pkmn | stat = stat}

update_newpkmn_yield : String -> NewPkmn -> NewPkmn
update_newpkmn_yield yieldstr pkmn =
  let 
    yield = String.toInt yieldstr
      |> Maybe.map clamp_pkmn_ev_yield
      |> Maybe.withDefault 1
  in
  { pkmn | yield = yield }

update_target_ev : StatSet -> Stat -> String -> StatSet
update_target_ev set stat textval = 
  let
    set_val = set_setstat_val set stat 
    val = String.toInt textval
      |> Maybe.map clamp_ev
      |> Maybe.withDefault 0
    maxNewSet = set_val val
    minOldSet =  set_val 0
    maxPossible =  510 - sum_evs_statset minOldSet
    clampedVal = min val maxPossible
  in
    set_val clampedVal


----------
-- View --
view : Model -> Html.Html Msg
view model = 
  div [ A.id "Tracker" ]
    [ h1 [] [ text "Sw/Sh EV Tracker" ]
    , ev_status model
    , div [ A.id "Counter" ]
      [ h2 [] [ text "Options" ]
      , options_div model
      , h2 [] [ text "Wild Pokemon" ]
      , wild_pkmn model
      ]
    ]

-- View => Wild Pokemon List --
wild_pkmn : Model -> Html.Html Msg
wild_pkmn model =
  let 
    partial_get_remaning = get_remaining model
  in
  div [A.id "WildPkmn"] 
  [ ul [] (map_pkmn_list model.pkmnList partial_get_remaning)
  , add_pkmn_button model.addPkmn
  ]

map_pkmn_list : List PKMN -> (PKMN -> List (Stat, Int)) -> List (Html.Html Msg)
map_pkmn_list pkmns calc_remaining = 
  List.map (pkmn_to_li calc_remaining) pkmns

pkmn_to_li : (PKMN -> List (Stat, Int)) -> PKMN -> Html.Html Msg
pkmn_to_li calc_remaining pkmn = 
  let
    name = Maybe.withDefault "" pkmn.name
    yield = "+" ++ String.fromInt pkmn.yield ++ " " ++ stat_to_str pkmn.stat
    count = "KOed " ++ String.fromInt pkmn.count
    koLeft = p_remaining_evs <| calc_remaining pkmn
    class = pkmn_li_class pkmn
  in
  li [onClick <| KilledPkmn pkmn.id, A.class class] 
    [ button 
      [ onClick <| RemovePkmn pkmn.id
      , A.class "pkmn-x"
      , A.class "neg-button"
      ] [text "X"] 
    , p [A.class "pkmn-name"] [text name]
    , p [A.class "pkmn-yield"] [text yield]
    , p [A.class "pkmn-kos"] [text count]
    , koLeft
    , p [A.class "plus-text"] [text "+"]
    ] 
    
pkmn_li_class : PKMN -> String 
pkmn_li_class pkmn = 
  String.toLower <| stat_to_str pkmn.stat 

p_remaining_evs : List (Stat, Int) -> Html.Html Msg
p_remaining_evs remainings = 
  div [A.class "pkmn-left"]
    (List.map (\r-> p [] [text (fmt_remaining_ev r)]) remainings)

fmt_remaining_ev : (Stat, Int) -> String
fmt_remaining_ev (stat, left) =
  (String.fromInt left) ++ " remaining [" ++ (stat_to_str stat) ++ "]"

get_remaining : Model -> PKMN -> List (Stat, Int)
get_remaining model pkmn = 
  -- store all of the possible yields (replace with Dict.fromList once pkmn yield evs is a list)
  let
    get_stat_remaining = diff_target_earned model.targetEvs model.earnedEvs
      |> calculate_remaining
    
    pokerusMult = if model.pokerus then 2 else 1

    stat_comp = stat_comparable pkmn.stat
    yielded = Dict.singleton stat_comp pkmn.yield
      |> (add_ev_item_yield model.item)
      |> Dict.toList
      |> List.map (\(s, y) -> (comparable_to_stat s, y * pokerusMult))
  in
    yielded
    |> List.map get_stat_remaining
    |> List.filterMap identity


add_ev_item_yield : Maybe Stat -> Dict Int Int -> Dict Int Int
add_ev_item_yield held dict =
  let
    held_comp = Maybe.map stat_comparable held
  in
  case held_comp of
    Just item -> 
      Dict.update item ((inc_ev_yield 8)) dict
    Nothing ->
      dict

inc_ev_yield : Int -> Maybe Int -> Maybe Int
inc_ev_yield itemYield prior =
  case prior of
    Just val -> Just (val + itemYield)
    Nothing -> Just itemYield 


-- View => Add New Pokemon --
add_pkmn_button : Maybe NewPkmn -> Html.Html Msg
add_pkmn_button pkmn =
  let 
    addNewButton = button 
      [onClick StartAddingNewPkmn
      , A.class "pos-button"
      , A.class "big"
      ] [text "+ New Pokemon"]
    addModal = 
      pkmn
      |> Maybe.map add_pkmn_popup
      |> Maybe.withDefault addNewButton
  in
  div [A.id "AddNewPkmn"] [addModal]

add_pkmn_popup : NewPkmn -> Html.Html Msg
add_pkmn_popup pkmn =
  let 
    name = Maybe.withDefault "" pkmn.name
    stat = stat_to_str pkmn.stat
    yield = String.fromInt pkmn.yield
  in
  div [A.id "AddPkmnPopup"]
  [ div [A.class "two-span"] 
    [ label [A.for "NewPkmnName"] [text "Name: "] 
    , input 
        [A.id "NewPkmnName"
        , A.type_ "text"
        , A.placeholder "Optional"
        , A.value name
        , onInput (\s -> NewPkmnUpdate (NewPkmnName s)) 
        ] []
    ]
  , div [A.class "one-span"]
    [ label [A.for "NewPkmnStat"] [text "Stat: "]
    , stat_selector "NewPkmnStat" stat
    ]
  , div [A.class "one-span"]
    [ label [A.for "NewPkmnYield"] [text "EV Yield: "]
    , ev_selector "NewPkmnYield" yield
    ]
  , button 
    [ onClick CancelAddingNewPkmn
    , A.class "neg-button"
    , A.class "big"
    ] [text "Cancel"]
  , button 
    [ onClick AddNewPkmnToList
    , A.class "pos-button"
    , A.class "big"
    ] [text "Add"]
  ]

stat_selector : String -> String -> Html.Html Msg
stat_selector id current =
  select 
    [A.id id
    , A.value current
    , onInput (\s -> NewPkmnUpdate (NewPkmnStat s))
    ] stat_options

stat_options =
  all_stat_list
  |> List.map stat_to_str
  |> List.map str_to_option_elem

ev_selector : String -> String -> Html.Html Msg
ev_selector id cur =
  let
    options = [1, 2, 3]
      |> List.map String.fromInt
      |> List.map str_to_option_elem
  in
  select 
    [A.id id
    , A.value cur
    , onInput (\s -> NewPkmnUpdate (NewPkmnYield s))
    ] options
  

-- View => Current EV Status --
ev_status model =
  div [A.id "EVs"] 
  [h2 [] [text "EV Spread"]
  , ev_table model.earnedEvs model.targetEvs
  , button 
    [ onClick ResetEarnedEvs
    , A.class "neg-button"
    , A.class "big"
    ] [text "Reset Earned EVs"]
  , button 
    [ onClick ResetTargetEvs
    , A.class "neg-button"
    , A.class "big"
    ] [text "Reset Target EVs"]
  ]


ev_table : StatSet -> StatSet -> Html.Html Msg
ev_table earned target = 
  let 
    create_stat_cell = display_stat_evs earned target
    tableElems = all_stat_list
      |> List.map create_stat_cell
  in
  div [A.id "EvTable"] tableElems

display_stat_evs : StatSet -> StatSet -> Stat -> Html.Html Msg
display_stat_evs earned target stat = 
  let 
    earnedVal = get_stat_value earned stat
    earnedValStr = String.fromInt earnedVal
    targetVal = get_stat_value target stat
    targetValStr = String.fromInt targetVal
    statusClass = ev_status_class earnedVal targetVal
    statName = stat_to_str stat
    statInputId = statName ++ "Input"
    labelStr = earnedValStr ++ " of "
  in
  div [ A.id statName, A.class "ev-grid", A.class statusClass] 
  [ label [A.for statInputId, A.class "ev-row-header"] [text statName]
  , label [A.for statInputId] [text labelStr]
  , input 
    [ A.type_ "number"
    , A.value targetValStr
    , A.id statInputId
    , onInput (SetEvTarget stat)
    ] []
  , button [onClick <| EvBerry stat, A.class "neg-button"] [text "- EV Berry"]
  , button [onClick <| SetEvTarget stat "252", A.class "pos-button"] [text "Max"]
  ]

ev_status_class : Int -> Int -> String
ev_status_class earned target = 
  if target == 0 then
    "zeroed"
  else
    case compare earned target of
      LT -> "not-reached"
      EQ -> "meet"
      GT -> "issue"

-- View => Options
options_div : Model -> Html.Html Msg
options_div model = 
   div [A.id "Options"]
    [ label [A.for "ItemInput"] [ text "Power Item: " ]
    , select [ A.id "ItemInput", onInput ChangeItem ] held_item_list
    , pokerus_toggle model
    ]

pokerus_toggle : Model -> Html.Html Msg
pokerus_toggle model = 
  div [ A.id "Pokerus" ]
  [ input [A.type_ "checkbox", A.id "PokerusCheck"] []
  , label 
    [A.for "PokerusCheck"
    , A.checked model.pokerus
    , onClick <| TogglePokerus ""
    ] [text "Pokerus"]
  ]

held_item_list = 
  List.map str_to_option_elem 
    [ "None"
    , "HP - Power Weight"
    , "Attack - Power Bracer"
    , "Defense - Power Belt"
    , "Special Attack - Power Lens"
    , "Special Defense - Power Band"
    , "Speed - Power Anklet"
    ]


-- Utils
item_str_to_stat s =
  case s of
    "None" -> Nothing
    "HP - Power Weight" -> Just HP
    "Attack - Power Bracer" -> Just Att
    "Defense - Power Belt" -> Just Def
    "Special Attack - Power Lens" -> Just SpA
    "Special Defense - Power Band" -> Just SpD
    "Speed - Power Anklet" -> Just Spe
    _ -> Nothing

stat_to_str : Stat -> String
stat_to_str stat =
  case stat of
    HP -> "HP"
    Att -> "Att"
    Def -> "Def"
    SpA -> "SpA"
    SpD -> "SpD"
    Spe -> "Spe"

str_to_stat : String -> Maybe Stat
str_to_stat str =
  case str of
    "HP" -> Just HP
    "Att" -> Just Att
    "Def" -> Just Def
    "SpA" -> Just SpA
    "SpD" -> Just SpD
    "Spe" -> Just Spe
    _ -> Nothing

all_stat_list : List Stat
all_stat_list = [ HP, Att, Def, SpA, SpD, Spe ]

stat_comparable : Stat -> Int
stat_comparable stat =
  case stat of
    HP -> 0
    Att -> 1
    Def -> 2
    SpA -> 3
    SpD -> 4
    Spe -> 5

comparable_to_stat : Int -> Stat
comparable_to_stat stat_comp =
  case stat_comp of
    0 -> HP
    1 -> Att
    2 -> Def
    3 -> SpA
    4 -> SpD
    5 -> Spe
    _ -> HP -- this should never happen

-- Utils => StatSet --
zero_statset : StatSet
zero_statset = 
  StatSet 0 0 0 0 0 0

sum_evs_statset : StatSet -> Int
sum_evs_statset set = 
  set.hp + set.att + set.def + set.spa + set.spd + set.spe

set_setstat_val : StatSet -> Stat -> Int -> StatSet
set_setstat_val set stat val = 
  case stat of
    HP  -> { set | hp = val }
    Att -> { set | att = val }
    Def -> { set | def = val }
    SpA -> { set | spa = val }
    SpD -> { set | spd = val }
    Spe -> { set | spe = val }

get_stat_value : StatSet -> Stat -> Int
get_stat_value set stat =
  case stat of 
    HP -> set.hp
    Att -> set.att
    Def -> set.def
    SpA -> set.spa
    SpD -> set.spd
    Spe -> set.spe

iter_statset : StatSet -> List (Stat, Int)
iter_statset set =
  all_stat_list
  |> List.map (\stat -> (stat, get_stat_value set stat))

-- Map between stat and evs left to reach target. 
-- The dict only includes stats that were set by the user (non-zero model.targetEvs)
type alias LeftStatDict = Dict Int Int

diff_target_earned : StatSet -> StatSet -> LeftStatDict
diff_target_earned target earned =
  iter_statset target
  |> List.filter (\(_, y) -> y /= 0)
  |> List.map (\(s, y) -> (s, y - (get_stat_value earned s)))
  |> List.map (\(s, y) -> (stat_comparable s, y))
  |> Dict.fromList

-- Calculate the number of KOs needed to inc `earned` to `target`
calculate_remaining : LeftStatDict -> (Stat, Int) -> Maybe (Stat, Int)
calculate_remaining evsLeft (stat, yield) =
  let
    statComp = stat_comparable stat
    number_remaining evs = 
      toFloat evs
      |> (/) (toFloat yield)
      |> (/) 1
      |> ceiling
      |> (Tuple.pair stat)
  in
  Dict.get statComp evsLeft
  |> Maybe.map number_remaining

-- ==> Clamp EV delta `n` to that max value that StatSet `set` can add
max_possible_ev : StatSet -> Int -> Int
max_possible_ev set n =
  min n <| max 0 (510 - (sum_evs_statset set)) 

str_to_option_elem str = 
  option [] [text str]

clamp_pkmn_ev_yield = clamp 1 3

clamp_ev val = 
  clamp 0 252 val
