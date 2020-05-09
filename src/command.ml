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

exception InvalidDuration

exception InvalidWeek

type view_option = Single of Calendar.event list | Week of Time.t

type meta_command = Create | Access

type command = 
  | Create
  | Add 
  | Delete 
  | Edit 
  | View
  | Save
  | Exit

(** [first_3 t] is the first element in tuple of size 3 [t]. *)
let first_3 t = match t with (x, _, _) -> x

(** [second_3 t] is the second element in tuple of size 3 [t]. *)
let second_3 t = match t with (_, x, _) -> x

(** [third_3 t] is the third element in tuple of size 3 [t]. *)
let third_3 t = match t with (_, _, x) -> x

(** [first_4 t] is the first element in tuple of size 4 [t]. *)
let first_4 t = match t with (x, _, _, _) -> x

(** [second_4 t] is the second element in tuple of size 4 [t]. *)
let second_4 t = match t with (_, x, _, _) -> x

(** [third_4 t] is the third element in tuple of size 4 [t]. *)
let third_4 t = match t with (_, _, x, _) -> x

(** [fourth_4 t] is the fourth element in tuple of size 4 [t]. *)
let fourth_4 t = match t with (_, _, _, x) -> x

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
    Raises: [InvalidWeek] if the date entered is not logical or the 
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
  with _ -> raise InvalidWeek


let main_parse str = 
  let str' = String.trim str in
  let n = String.length str' in
  let lc_str = String.lowercase_ascii str' in
  if n = 3 then (if lc_str = "add" then Add else raise CommandDNE) 
  else if n = 6 then (if lc_str = "delete" then Delete else raise CommandDNE)
  else if n = 4 then 
    (if lc_str = "edit" then Edit
     else if lc_str = "view" then View 
     else if lc_str = "save" then Save
     else if lc_str = "exit" then Exit
     else raise CommandDNE)
  else raise CommandDNE

(** [is_valid_date_string str] is true if [str] is a valid start or end time string. *)
let is_valid_date_string str = 
  try 
    Time.from_input_string str |> Time.is_valid
  with _ -> false

let handle_date_input t1 t2 = 
  if not (Time.is_valid t2) then raise InvalidDate 
  else if not (Time.occurs_before t1 t2) then raise StartAfterEnd 
  else t2

(** [validate_duration str] is {hour=h; minute=m;} corresponding to the user's
    input duration [str]. If [str] has no ':', then it is assumed that the
    inputted duration is referring to hours, with minutes 0. If [str] 
    contains ':' then h:m is the input string. 
    Raises: [InvalidDuration] if the user's input cannot be interpreted 
    a number *)
let validate_duration (str : string) : Time.time_d = 
  (* handle just hours inputted *)
  if not (String.contains str ':') 
  then {hour = int_of_string str; minute = 0}

  (* handle hours and minutes inputted *)
  else let split = String.split_on_char ':' str in 
    if not (List.length split = 2) then raise InvalidDuration 
    else try
        let hour = int_of_string (List.nth split 0) in 
        let min = int_of_string (List.nth split 1) in 
        if not (hour >= 0) then raise InvalidDuration 
        else if not (min >= 0 && min <60) then raise InvalidDuration
        else {hour = hour; minute = min}
      with _ -> raise InvalidDuration

let duration_parse t dur = 
  Time.increment_duration t (validate_duration dur)

let add_parse input : Calendar.event = 
  {
    starts = second_4 input |> Time.toGMT;
    ends = third_4 input |> Time.toGMT;
    name = first_4 input;
    description = fourth_4 input;
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


let edit_parse input = 
  try
    let eventToEdit = first_3 input in 
    let fieldToEdit = ensure_valid_field (second_3 input) in 
    let newField = ensure_valid_change (third_3 input) fieldToEdit in

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

let view_parse c str = 

  (* Handle view week *)
  if String.contains str '/' 
     || 
     (match int_of_string_opt str with | Some _ -> true | None -> false) then 

    let week_info = today_smart str in 
    let t = {
      year = third_3 week_info;
      month = second_3 week_info;
      day_m = first_3 week_info;
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
