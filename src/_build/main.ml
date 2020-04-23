(**
let print_InvalidDate x = 
  ANSITerminal.(print_string [red] "The date you entered does not exist. Enter again. \n")

let print_CommandDNE x = 
  ANSITerminal.(print_string [red] "That command does not exist.\n")

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

let print_EmptyEventName x = 
  ANSITerminal.(print_string [red] "Enter a non empty name for this event.\n")

let print_StartAfterEnd x = 
  ANSITerminal.(print_string [red] "Make sure the starting time is before the end time!\n")

let print_InvalidField x = 
  ANSITerminal.(print_string [red] "Make sure you are changing the correct field. \n")

let rec main_instructions x = 
  ANSITerminal.(print_string [black] "What would you like to do?\n");
  Stdlib.print_string "Type here: > ";
  try (Stdlib.read_line ()|> main_parse) 
  with 
  | CommandDNE -> print_CommandDNE;
    main_instructions x

let event_name_instructions x = 
  ANSITerminal.(print_string [black] "What is the name of this event?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let event_delete_edit_name_instructions action = 
  ANSITerminal.(print_string [black] ("What is the name of the event to " ^ action ^ "?\n"));
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let start_time_instructions x = 
  ANSITerminal.(print_string [black] "When does this event start?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let end_time_instructions x = 
  ANSITerminal.(print_string [black] "When does this event end?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let description_instructions x = 
  ANSITerminal.(print_string [black] "What's this event's description?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let field_instructions x = 
  ANSITerminal.(print_string [black] "What do you want to edit - name, start, end, or description?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let field_edit_instructions field = 
  ANSITerminal.(print_string [black] ("What would you like to change the " ^ field ^ " to?\n"));
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

let rec add_instructions x = 
  let name = event_name_instructions 1 in 
  let starttime = start_time_instructions 1 in 
  let endtime = end_time_instructions 1 in 
  let description = description_instructions 1 in
  try 
    (add_parse [name; starttime; endtime; description])
  with 
  | InvalidDate -> print_InvalidDate; add_instructions 1
  | MalformedList -> print_MalformedList; add_instructions 1
  | EmptyEventName -> print_EmptyEventName; add_instructions 1
  | StartAfterEnd -> print_StartAfterEnd; add_instructions 1
  | InvalidDateString -> print_InvalidDateString; add_instructions 1

let rec delete_instructions x = 
  let name = event_delete_edit_name_instructions "delete" in 
  let starttime = start_time_instructions 1 in 
  try delete_parse [name; starttime] 
  with 
  | InvalidDate -> print_InvalidDate; delete_instructions 1 
  | MalformedList -> print_MalformedList; delete_instructions 1
  | EmptyEventName -> print_EmptyEventName; delete_instructions 1
  | StartAfterEnd -> print_StartAfterEnd; delete_instructions 1
  | InvalidDateString -> print_InvalidDateString; delete_instructions 1

let rec edit_instructions x = 
  let name = event_delete_edit_name_instructions "edit" in 
  let starttime = start_time_instructions 1 in 
  let field = field_instructions 1 in 
  let change = field_edit_instructions field in 
  try edit_parse [name; starttime; field; change] 
  with 
  | InvalidDate -> print_InvalidDate; edit_instructions 1
  | MalformedList -> print_MalformedList; edit_instructions 1
  | EmptyEventName -> print_EmptyEventName; edit_instructions 1
  | StartAfterEnd -> print_StartAfterEnd; edit_instructions 1
  | InvalidDateString -> print_InvalidDateString; edit_instructions 1

let stoppingpoint x = Stdlib.print_string ("heres where we stop");

(** [play c] is the primary recursive function for playing this application.*) 
let play c = 
  match main_instructions 1 with  
  (* | Add -> add_instructions |> Calendar.add_event c *)
  | Add -> add_instructions |> stoppingpoint
  (* | Delete -> delete_instructions |> Calendar.delete_event c *)
  | Delete -> delete_instructions |> stoppingpoint
  (* | Edit -> edit_instructions |> Calendar.edit_event c *)
  | Edit -> edit_instructions |> stoppingpoint
  | _ -> ANSITerminal.(print_string [red] "taf");
*)
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Adaptive Calender system.\n");
  print_endline "What do you want to do?\n"
  (* Stdlib.print_string  "> ";
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> () *)
  (*play 1 *)

(* Execute the game engine. *)
let () = main ()