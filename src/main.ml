open ANSITerminal
open Command 
open Calendar 

let print_success x = 
  ANSITerminal.(print_string [red] "Success! \n")

let print_try_again x = 
  ANSITerminal.(print_string [red] "Try again. \n")

let print_InvalidDate x = 
  ANSITerminal.(print_string [red] "The date you entered does not exist. \n")

let print_MetaCommandDNE x = 
  ANSITerminal.(print_string [red] "Enter 'create' or 'access'.\n")

let print_CommandDNE x = 
  ANSITerminal.(print_string [red] "That command does not exist.\n")

let print_EventDNE x = 
  ANSITerminal.(print_string [red] "That event doesnt exist. Make sure you have the correct event name and start time.\n")

let print_InvalidDateString x : unit = 
  ANSITerminal.(print_string [red] ("The date you entered is not in the correct form. 
   Make sure you enter a date in the form 'mm/dd/yyyy/hh:ii/xx where:' \n" 
                                    ^  "mm is the month in range [01..12]\n"
                                    ^  "dd is the day in range [01..31]\n"
                                    ^  "yyyy is the year in range [2020..]\n"
                                    ^  "hh is the hour ins range [01..12]\n"
                                    ^  "ii is the minute in range [00..59]\n"
                                    ^  "xx is 'am' or 'pm'\n"))

let print_MalformedList x = 
  ANSITerminal.(print_string [red] "This shouldnt happen.\n")

let print_CannotAddExisting x = 
  ANSITerminal.(print_string [red] "You cannot add an event with the same name and start date as another event. Try again. \n")

let print_EmptyEventName x = 
  ANSITerminal.(print_string [red] "Enter a non empty name for this event.\n")

let print_StartAfterEnd x = 
  ANSITerminal.(print_string [red] "Make sure the starting time is before the end time!\n")

let print_InvalidField x = 
  ANSITerminal.(print_string [red] "Make sure you are changing the correct field. \n")

let rec main_instructions x = 
  ANSITerminal.(print_string [red] "What would you like to do?\n");
  Stdlib.print_string "Type here: > ";
  try (Stdlib.read_line ()|> main_parse) 
  with 
  | CommandDNE -> print_CommandDNE 1;
    main_instructions x

let event_name_instructions x = 
  ANSITerminal.(print_string [red] "What is the name of this event?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let event_delete_edit_name_instructions action = 
  ANSITerminal.(print_string [red] ("What is the name of the event to " ^ action ^ "?\n"));
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let start_time_instructions x = 
  ANSITerminal.(print_string [red] "When does this event start?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let end_time_instructions x = 
  ANSITerminal.(print_string [red] "When does this event end?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let description_instructions x = 
  ANSITerminal.(print_string [red] "What's this event's description?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let field_instructions x = 
  ANSITerminal.(print_string [red] "What do you want to edit - name, start, end, or description?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let field_edit_instructions field = 
  ANSITerminal.(print_string [red] ("What would you like to change the " ^ field ^ " to?\n"));
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let rec create_instructions x : string =
  ANSITerminal.(print_string [red] ("What is the name of your new calendar? Make sure you do not choose a name of an existing calendar.\n"));
  Stdlib.print_string "Type here: > ";
  try
    Stdlib.read_line () |> Command.create_parse
  with 
  | EmptyCalendarName -> print_try_again; create_instructions 1

let rec add_instructions x : Calendar.event = 
  let name = event_name_instructions 1 in 
  let starttime = start_time_instructions 1 in 
  let endtime = end_time_instructions 1 in 
  let description = description_instructions 1 in
  try 
    (add_parse [name; starttime; endtime; description])
  with 
  | InvalidDate -> print_InvalidDate 1; add_instructions 1
  | MalformedList -> print_MalformedList 1; add_instructions 1
  | EmptyEventName -> print_EmptyEventName 1; add_instructions 1
  | StartAfterEnd -> print_StartAfterEnd 1; add_instructions 1
  | InvalidDateString -> print_InvalidDateString 1; add_instructions 1

let rec delete_instructions x = 
  let name = event_delete_edit_name_instructions "delete" in 
  let starttime = start_time_instructions 1 in 
  try delete_parse [name; starttime] 
  with 
  | InvalidDate -> print_InvalidDate 1; delete_instructions 1 
  | MalformedList -> print_MalformedList 1; delete_instructions 1
  | EmptyEventName -> print_EmptyEventName 1; delete_instructions 1
  | StartAfterEnd -> print_StartAfterEnd 1; delete_instructions 1
  | InvalidDateString -> print_InvalidDateString 1; delete_instructions 1
  | EventDNE -> print_EventDNE 1; delete_instructions 1

let rec edit_instructions x = 
  let name = event_delete_edit_name_instructions "edit" in 
  let starttime = start_time_instructions 1 in 
  let field = field_instructions 1 in 
  let change = field_edit_instructions field in 
  try edit_parse [name; starttime; field; change] 
  with 
  | InvalidDate -> print_InvalidDate 1; print_try_again 1; edit_instructions 1
  | MalformedList -> print_MalformedList 1; print_try_again 1; edit_instructions 1
  | EmptyEventName -> print_EmptyEventName 1; print_try_again 1; edit_instructions 1
  | StartAfterEnd -> print_StartAfterEnd 1; print_try_again 1; edit_instructions 1
  | InvalidDateString -> print_InvalidDateString 1; print_try_again 1; edit_instructions 1
  | EventDNE -> print_EventDNE 1; print_try_again 1; edit_instructions 1

(** [play c] is the primary recursive function for playing this application.*) 
let rec change (success : bool) (c : Calendar.t) : unit = 
  if success then print_success 1;
  try
    match main_instructions 1 with  
    | Create -> let n = create_instructions 1 in change true (Calendar.empty n)
    | Add -> add_instructions 1 |> Calendar.add_event c |> change true
    | Delete -> delete_instructions 1 |> Calendar.delete_event c |> change true
    | Edit -> edit_instructions  1|> Calendar.edit_event c |> change true
    | Save -> Command.save_parse c; print_success 1; exit 0
    | _ -> change false c
  with 
  | CannotAddExisting -> print_CannotAddExisting 1; print_try_again 1; change false c
  | EventDNE -> print_EventDNE 1; print_try_again 1; change false c

let start_cal file = 
  let c = file |> Yojson.Basic.from_file |> Calendar.from_json in 
  change false c

let read_file x = 
  print_endline "Enter the calendar you would like to edit:\n";
  Stdlib.print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | cal_name -> start_cal (cal_name ^ ".json")

let rec meta_instructions x = 
  ANSITerminal.(print_string [red] "Would you like to create a new calendar or access an existing one?\n");
  ANSITerminal.(print_string [red] "Enter 'create' or 'access'\n");
  Stdlib.print_string "Type here: > ";
  try 
    let meta = (Stdlib.read_line ()|> meta_parse) in 
    if meta = "access" then read_file 1;
    if meta = "create" then let n = create_instructions 1 in 
      change true (Calendar.empty n)
  with 
  | MetaCommandDNE -> print_MetaCommandDNE 1;
    meta_instructions x

(** [main ()] prompts for the game to play, then starts it. *)
(* let main () =
   ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Adaptive Calender system.\n");
   print_endline "Enter the calendar you would like to edit:\n";
   Stdlib.print_string  "> ";

   match read_line () with
   | exception End_of_file -> ()
   | file_name -> start_cal file_name *)

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Adaptive Calender system!\n");
  meta_instructions 1

(* Execute the game engine. *)

let () = main ()