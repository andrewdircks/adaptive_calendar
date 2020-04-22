open Calendar
open Time

exception CommandDNE

exception InvalidDate

exception InvalidDateString

exception MalformedList

exception EmptyEventName

exception StartAfterEnd

type command = 
  | Add 
  | Delete 
  | Edit 
  | View
  | Previous
  | Next
  | Save

let main_parse str = 
  let n = String.length str in
  let lc_str = String.lowercase_ascii str in
  if n = 3 then (if lc_str = "add" then Add else raise CommandDNE) 
  else if n = 6 then (if lc_str = "delete" then Delete else raise CommandDNE)
  else if n = 8 then (if lc_str = "previous" then Previous else raise CommandDNE)
  else if n = 4 then 
    (if lc_str = "edit" then Edit
     else if lc_str = "view" then View 
     else if lc_str = "next" then Next
     else if lc_str = "save" then Save
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
        int_of_string (String.sub str 0 2);
        int_of_string (String.sub str 3 2);
        int_of_string (String.sub str 6 4);
        int_of_string (String.sub str 11 2);
        int_of_string (String.sub str 14 2);
        true
      with Failure _ -> false

(** [handle_date_input s1 s2] is the tuple (d1,d2) where d1 and d2 are the 
    respective date/time values for date strings s1 and s2.
    Raises: [InvalidDateString] if [s1] or [s2] is not in proper form. 
            [InvalidDate] if [s1] or [s2] is not a valid date. 
            [StartAfterEnd] if [s1] is not before [s2]. *)
let handle_date_input s1 s2 = 
  (* determine if s1 and s2 are valid date strings *)
  if not (is_valid_date_string s1) then raise InvalidDateString
  else if not (is_valid_date_string s1) then raise InvalidDateString 
  else 
    let d1 = Time.from_string s1 in 
    let d2 = Time.from_string s2 in
    (* determine if d1 and d2 are valid dates *)
    if not (Time.is_valid d1) || not (Time.is_valid d2) then raise InvalidDate 
    (* determine if d1 occurs before d2 *)
    else if not (Time.occurs_before d1 d2) then raise StartAfterEnd 
    else (d1,d2)

let add_parse input = 
  if List.length input <> 4 then raise MalformedList
  else 
    (* handle event name input *)
    let name = List.hd input in 
    if String.length name = 0 then raise EmptyEventName
    (* handle start and end time input *)
    else 
      let start = List.hd (List.tl input) in 
      let en = List.hd (List.tl (List.tl input)) in 
      let timeframe = handle_date_input start en in 
      (* handle description input *)
      let description = List.hd List.tl ((List.tl (List.tl input))) in 

      {
        starts = fst timeframe;
        ends = snd timeframe;
        name = name;
        description = description;
      }

let delete_parse input = 
  if List.length input <> 2 then raise MalformedList
  else 
    (* handle event name input *)
    let name = List.hd input in 
    if String.length name = 0 then raise EmptyEventName 
    else 
      (* handle start time input *)
      let start = List.hd (List.tl input) in 
      if not (is_valid_date_string start) then raise InvalidDateString
      else 
        let d = Time.from_string start in
        if not (Time.is_valid d1) then raise InvalidDate 
        else (name,d)





