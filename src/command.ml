open Calendar
open Time

exception MetaCommandDNE

exception CommandDNE

exception InvalidDate

exception InvalidDateString

exception MalformedList

exception EmptyEventName

exception StartAfterEnd

exception InvalidField

exception EmptyCalendarName

exception OutOfBounds

type view_option = Single of Calendar.event list | Week of Time.t

type meta_command = Create | Access

type command = 
  | Create
  | Add 
  | Delete 
  | Edit 
  | View
  | Previous
  | Next
  | Save
  | Exit

let meta_parse str = 
  let n = String.length str in
  let lc_str = String.lowercase_ascii str in
  if n = 6 then 
    if lc_str = "create" then "create"
    else if lc_str = "access" then "access"
    else raise MetaCommandDNE
  else raise MetaCommandDNE

(** [today_smart str] handles a user input date string. 
    If the string is in the form xx, where xx is in an integer between 0 and 31 
    then [(int_of_string xx, currentmonth, currentyear)] is returned. 
    If the string is in the form xx/yy, where yy is between 1 and 12, then 
    [(int_of_string xx, int_of_string yy, current year)] is returned. 
    If the string is in the form xx/yy/zzzz then all data is returned.
    Raises: Invalid date string if the date entered is not logical or the 
        string is not formed. Does not handle impossible combinations, i.e. 
        "02/31/" can be returned. *)
let today_smart (str : string) : Time.day_m * Time.month * Time.year = 
  try 
    let now = Time.now in
    let split = String.split_on_char '/' str in 
    let fields = List.length split in 

    (* handle only day entered *)
    if fields = 1 then 
      let day = List.nth split 0 |> int_of_string in 
      if day < 1 || day > 31 then failwith "esc"
      else (day, now.month, now.year)

    (* handle day and month entered *)
    else if fields = 2 then 
      let day = List.nth split 1 |> int_of_string in 
      let month = List.nth split 0 |> int_of_string |> Time.month_from_int in 
      if day < 1 || day > 31 then failwith "esc" 
      else (day, month, now.year)

    (* handle day, month, and year entered *)
    else if fields = 3 then 
      let day = List.nth split 1 |> int_of_string in 
      let month = List.nth split 0 |> int_of_string |> Time.month_from_int in 
      let year = List.nth split 2 |> int_of_string in
      if day < 1 || day > 31 then failwith "esc" 
      else if year < 2020 then failwith "esc"
      else (day, month, year)

    (* handle inproper number of fields entered *)
    else failwith "esc"

  (* catch int of string failures *)
  with _ -> raise InvalidDateString


let main_parse str = 
  let str' = String.trim str in
  let n = String.length str' in
  let lc_str = String.lowercase_ascii str' in
  if n = 3 then (if lc_str = "add" then Add else raise CommandDNE) 
  else if n = 6 then (if lc_str = "delete" then Delete else raise CommandDNE)
  else if n = 8 then (if lc_str = "previous" then Previous else raise CommandDNE)
  else if n = 4 then 
    (if lc_str = "edit" then Edit
     else if lc_str = "view" then View 
     else if lc_str = "next" then Next
     else if lc_str = "save" then Save
     else if lc_str = "exit" then Exit
     else raise CommandDNE)
  else raise CommandDNE

(** [is_valid_date_string str] is true if [str] is in the form 
    ii/ii/iiii/ii:ii/cc. where each 'i' is an integer and each 'c' is a char. *)
let is_valid_date_string str = 
  if String.length str <> 19 then false 
  (* check for correct slash position *)
  else if (str.[2] <> '/' || str.[5] <> '/' || str.[10] <> '/' || str.[16] <> '/')
  then false 
  (* check for correct colon position *)
  else if (str.[13] <> ':') then false
  (* check for am/pm at the end *)
  else
    let ampm = (String.sub str 17 2) in
    if (String.lowercase_ascii  ampm <> "am" && String.lowercase_ascii ampm <> "pm")
    then false
    (* check for integers in data/time spots *)
    else try
        let areints = int_of_string (String.sub str 0 2) +
                      int_of_string (String.sub str 3 2) +
                      int_of_string (String.sub str 6 4) +
                      int_of_string (String.sub str 11 2) +
                      int_of_string (String.sub str 14 2) in
        if areints > 0 then true else false
      with Failure _ -> false

(** [handle_date_input s1 s2] is the tuple (d1,d2) where d1 and d2 are the 
    respective date/time values for date strings s1 and s2.
    Raises: [InvalidDateString] if [s1] or [s2] is not in proper form. 
        [InvalidDate] if [s1] or [s2] is not a valid date. 
        [StartAfterEnd] if [s1] is not before [s2]. *)
let handle_date_input s1 s2 = 
  (* determine if s1 and s2 are valid date strings *)
  if not (is_valid_date_string s1) then raise InvalidDateString
  else if not (is_valid_date_string s2) then raise InvalidDateString 
  else 
    let asdf = Time.from_input_string s1 in 
    let d2 = Time.from_input_string s2 in
    (* determine if d1 and d2 are valid dates *)
    if not (Time.is_valid asdf) || not (Time.is_valid d2) then raise InvalidDate 
    (* determine if d1 occurs before d2 *)
    else if not (Time.occurs_before asdf d2) then raise StartAfterEnd 
    else (asdf,d2)

let add_parse input : Calendar.event = 
  if List.length input <> 4 then raise MalformedList
  else 
    (* handle event name input *)
    let name = List.nth input 0 in 
    if String.length name = 0 then raise EmptyEventName
    (* handle start and end time input *)
    else 
      let start = List.nth input 1 in 
      let en = List.nth input 2 in 
      let timeframe = handle_date_input start en in 
      (* handle description input *)
      let description = List.nth input 3 in 

      {
        starts = (fst timeframe) |> Time.toGMT;
        ends = (snd timeframe) |> Time.toGMT;
        name = name;
        description = description;
      }

let delete_parse input = 
  if List.length input <> 2 then raise MalformedList
  else 
    (* handle event name input *)
    let name = List.nth input 0 in 
    if String.length name = 0 then raise EmptyEventName 
    else 
      (* handle start time input *)
      let start = List.nth input 1 in 
      if not (is_valid_date_string start) then raise InvalidDateString
      else 
        let d = Time.from_input_string start in
        if not (Time.is_valid d) then raise InvalidDate 
        else (name, d |> Time.toGMT)

(** [ensure_valid_field f] is [f] in all lowercase if [f] is the string 
    "name" "description" "start" or "end".
    Raises: [InvalidField] if [f] is not either of these strings. *)
let ensure_valid_field f = 
  let lc = String.lowercase_ascii f in
  if (lc <> "description" && lc <> "name" && lc <> "start" && lc <> "end")
  then raise InvalidField else lc

(** [ensure_valid_change c f] is [c] if [c] is an appropriate change for field 
    [f].
    Requires: [f] is a valid field in all lowercase. 
    Raises: [InvalidEdit]  *)
let ensure_valid_change c f = 
  (* handle name editing *)
  if f = "name" then 
    (if c = "" then raise EmptyEventName else c) 

  (* handle start or end time editing *)
  else if f = "start" || f = "end" then 
    (if not (is_valid_date_string c) then raise InvalidDateString else c)

  (* descriptions always valid *)
  else c

let first t = match t with (x, _, _) -> x
let second t = match t with (_, x, _) -> x
let third t = match t with (_, _, x) -> x

let edit_parse input = 
  try
    let eventToEdit = first input in 
    let fieldToEdit = ensure_valid_field (second input) in 
    let newField = ensure_valid_change (third input) fieldToEdit in

    (* return respective tuple, note eventToEdit is already converted to GMT *)
    (eventToEdit, fieldToEdit, newField)

  with 
  | InvalidDate -> raise InvalidDate
  | MalformedList -> raise MalformedList 
  | EmptyEventName -> raise EmptyEventName 
  | InvalidDateString -> raise InvalidDateString
  | InvalidField -> raise InvalidField

let save_parse c = Calendar.to_json c

let create_parse name = 
  if name = "" then raise EmptyCalendarName
  else (Calendar.to_json (Calendar.empty name)); name

let first t = match t with (x, _, _) -> x
let second t = match t with (_, x, _) -> x
let third t = match t with (_, _, x) -> x

let view_parse c str = 

  (* Handle view week *)
  if String.contains str '/' 
     || 
     (match int_of_string_opt str with | Some _ -> true | None -> false) then 

    let week_info = today_smart str in 
    let t = {
      year = third week_info;
      month = second week_info;
      day_m = first week_info;
      time_d = {hour=0; minute=1}
    } in 
    Week t

  (* Handle view event *)
  else 
    match Calendar.find_event c str with 
    | Some es -> Single es
    | None -> raise Calendar.EventDNE

let multiple_event_parse es idx = 
  try List.nth es (idx - 1) 
  with _ -> raise OutOfBounds
