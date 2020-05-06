open ANSITerminal
open Command 
open Calendar 

(** [print_success ()] notifies user of a succesful operation *)
let print_success () = 
  ANSITerminal.(print_string [red] "Success! \n")

(** [print_try_again ()] displays a try again message, to handle
    unexpected user inputs. *)
let print_try_again () = 
  ANSITerminal.(print_string [red] "Try again. \n")

(** [print_InvalidDate ()] displays invalid date error message. *)
let print_InvalidDate () = 
  ANSITerminal.(print_string [red] "The date you entered does not exist. \n")

(** [print_MetaCommandDNE ()] displays meta command does not exist error message. *)
let print_MetaCommandDNE () = 
  ANSITerminal.(print_string [red] "Enter 'create' or 'access'.\n")

(** [print_CommandDNE ()] displays command does not exist error message. *)
let print_CommandDNE () = 
  ANSITerminal.(print_string [red] "That command does not exist.\n")

(** [print_EventDNE ()] displays event does not exist error message. *)
let print_EventDNE () = 
  ANSITerminal.(print_string [red] "That event doesnt exist. Make sure you have the correct event name and start time.\n")

(** [print_InvalidDateString ()] displays invalid date message. *)
let print_InvalidDateString () : unit = 
  ANSITerminal.(print_string [red] ("The date you entered is not in the correct form. 
   Make sure you enter a date in the form 'mm/dd/yyyy/hh:ii/xx where:' \n" 
                                    ^  "mm is the month in range [01..12]\n"
                                    ^  "dd is the day in range [01..31]\n"
                                    ^  "yyyy is the year in range [2020..]\n"
                                    ^  "hh is the hour ins range [01..12]\n"
                                    ^  "ii is the minute in range [00..59]\n"
                                    ^  "xx is 'am' or 'pm'\n"))

(** [print_MalformedList ()] displays malformed list message *)
let print_MalformedList () = 
  ANSITerminal.(print_string [red] "This shouldnt happen.\n")

(** [print_CannotAddExisting ()] displays error when user inputs empty string *)
let print_CannotAddExisting () = 
  ANSITerminal.(print_string [red] "You cannot add an event with the same name and start date as another event. Try again. \n")

(** [print_EmptyEventName ()] displays error when user inputs empty string *)
let print_EmptyEventName () = 
  ANSITerminal.(print_string [red] "Enter a non empty name for this event.\n")

(** [print_StartAfterEnd ()] displays error when start time occurs after end *)
let print_StartAfterEnd () = 
  ANSITerminal.(print_string [red] "Make sure the starting time is before the end time!\n")

(** [print_InvalidField ()] displays error for an invalid field. *)
let print_InvalidField () = 
  ANSITerminal.(print_string [red] "Make sure you are changing the correct field. \n")

(** [print_OutOfBounds ()] displays error for an invalid integer array index. *)
let print_OutOfBounds () = 
  ANSITerminal.(print_string [red] "Enter a valid integer from the list of start times. \n")

(** [print_multiple_events ()] prints a message that multiple events with this 
    name exist. *)
let print_multiple_events () = 
  ANSITerminal.(print_string [blue] "Multiple events with this name exist. When does your this start? (enter the number as labeled). \n")

(** [main_instructions ()] is the main recursive function once a calendar is selected.
    It enables the user to run functions that interact with the calendar.*)
let rec main_instructions () = 
  ANSITerminal.(print_string [Bold] "\nWhat would you like to do?\n");
  ANSITerminal.(print_string [blue] "Add: Make new event\n");
  ANSITerminal.(print_string [blue] "Edit: Modify existing event\n");
  ANSITerminal.(print_string [blue] "Delete: Delete existing event\n");
  ANSITerminal.(print_string [blue] "View: View events\n");
  ANSITerminal.(print_string [blue] "Save: Save your changes\n");
  ANSITerminal.(print_string [blue] "Exit: Exit the application (does not save)\n \n");

  Stdlib.print_string "Type here: > ";
  try (Stdlib.read_line ()|> main_parse) 
  with 
  | CommandDNE -> print_CommandDNE ();
    main_instructions ()

(** [event_name_instructions ()] prompts user for name of event. *)
let event_name_instructions () = 
  ANSITerminal.(print_string [red] "What is the name of this event?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [vevent_or_weel_instructions ()] prompts user to enter an event name or week to view. *)
let event_or_week_instructions () = 
  ANSITerminal.(print_string [green] "Enter the name on the event you would like to view.\n");
  ANSITerminal.(print_string [green] "Or, enter the starting date of the week you would like to view.\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [event_delete_edit_name_instructions action] prompts user for event name to perform
    [action] on. *)
let event_delete_edit_name_instructions action = 
  ANSITerminal.(print_string [red] ("What is the name of the event to " ^ action ^ "?\n"));
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [start_time_instructions ()] prompts user for start time value. *)
let start_time_instructions () = 
  ANSITerminal.(print_string [red] "When does this event start?\n");
  ANSITerminal.(print_string [blue] "Format: mm/dd/yyyy/hh:mm/am-pm\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [end_time_instructions ()] prompts user for end time value. *)
let end_time_instructions () = 
  ANSITerminal.(print_string [red] "When does this event end?\n");
  ANSITerminal.(print_string [blue] "Format: mm/dd/yyyy/hh:mm/am-pm\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [description_instructions ()] prompts user for event's description. *)
let description_instructions () = 
  ANSITerminal.(print_string [red] "What's this event's description?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [field_instructions ()] prompts user for field to change. *)
let field_instructions () = 
  ANSITerminal.(print_string [red] "What do you want to edit - name, start, end, or description?\n");
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [field_edit_instructions field] prompts user for changed value of [field]*)
let field_edit_instructions field = 
  ANSITerminal.(print_string [red] ("What would you like to change the " ^ field ^ " to?\n"));
  Stdlib.print_string "Type here: > ";
  Stdlib.read_line ()

(** [print_exit_message ()] prints the exit message. *)
let print_exit_message () = 
  ANSITerminal.(print_string [green] ("Exiting calendar. See you soon!\n"))


(** [create_instructions ()] runs instructions for creating calendar*)
let rec create_instructions () : string =
  ANSITerminal.(print_string [red] ("What is the name of your new calendar? Make sure you do not choose a name of an existing calendar.\n"));
  Stdlib.print_string "Type here: > ";
  try
    Stdlib.read_line () |> Command.create_parse
  with 
  | EmptyCalendarName -> print_try_again (); (create_instructions ())

let rec print_start_times es acc : unit = 
  match es with 
  | [] -> ()
  | h::t -> ANSITerminal.(print_string [red] ((string_of_int acc) ^ ". " ^ (Graphic.to_display_time (h.starts |> Time.toLocal)) ^ "\n"));
    print_start_times t (acc + 1)


(** [multiple_events_instructions es] runs instructions for handling events
    with the same name. *)
let rec multiple_events_instructions es display : Calendar.event = 
  if display then print_multiple_events ();
  print_start_times es 1; 
  Stdlib.print_string "Type here: > ";
  try 
    Stdlib.read_line () |> int_of_string |> Command.multiple_event_parse es 
  with 
  | OutOfBounds -> print_OutOfBounds (); multiple_events_instructions es false

(** [add_instructions ()] runs instructions for adding events*)
let rec add_instructions () : Calendar.event = 
  let name = event_name_instructions () in 
  let starttime = start_time_instructions () in 
  let endtime = end_time_instructions () in 
  let description = description_instructions () in
  try 
    (add_parse [name; starttime; endtime; description])
  with 
  | InvalidDate -> print_InvalidDate (); add_instructions ()
  | MalformedList -> print_MalformedList (); add_instructions ()
  | EmptyEventName -> print_EmptyEventName (); add_instructions ()
  | StartAfterEnd -> print_StartAfterEnd (); add_instructions ()
  | InvalidDateString -> print_InvalidDateString (); add_instructions ()

let rec handle_multiple_events c name : Calendar.event = 
  (match Calendar.find_event c name with 
   | Some es -> (match es with 
       | h::[] -> h 
       | h::t -> multiple_events_instructions es true
       | _ -> print_OutOfBounds (); handle_multiple_events c name)
   | None -> print_EventDNE (); handle_multiple_events c name)

let rec delete_instructions (c : Calendar.t) : Calendar.t = 
  let name = event_delete_edit_name_instructions "delete" in 
  let event = handle_multiple_events c name
  in
  Calendar.delete_event c event

(** [edit_instructions ()] runs instructions for editing events*)
let rec edit_instructions c = 
  let name = event_delete_edit_name_instructions "edit" in 
  let event = handle_multiple_events c name in
  let field = field_instructions () in 
  let thechange = field_edit_instructions field in 
  try edit_parse (event, field, thechange)
  with 
  | InvalidDate -> print_InvalidDate (); print_try_again (); edit_instructions c
  | MalformedList -> print_MalformedList (); print_try_again (); edit_instructions c
  | EmptyEventName -> print_EmptyEventName (); print_try_again (); edit_instructions c
  | StartAfterEnd -> print_StartAfterEnd (); print_try_again (); edit_instructions c
  | InvalidDateString -> print_InvalidDateString (); print_try_again (); edit_instructions c
  | EventDNE -> print_EventDNE (); print_try_again (); edit_instructions c

(** [view_instructions c] runs instructions for viewing events of [c]*)
let rec view_instructions c inst : unit = 
  let command = event_or_week_instructions () in 
  try 
    match view_parse c command with 
    | Single es -> 
      (match es with 
       | [] -> failwith "find event error"
       | h::[] -> Graphic.view_event h; change false c
       | _ -> multiple_events_instructions es true |> Graphic.view_event; change false c )
    | Week t -> Graphic.view_week c t; change false c 
  with
  | EventDNE -> print_try_again (); view_instructions c false
  | InvalidDateString -> print_InvalidDateString (); print_try_again (); view_instructions c false
  | _ -> print_try_again (); view_instructions c false

(** [change success c] is the primary recursive function for playing this application.*) 
and change (success : bool) (c : Calendar.t) : unit = 
  if success then print_success ();
  try
    match main_instructions () with  
    | Create -> let n = create_instructions () in change true (Calendar.empty n)
    | Add -> add_instructions () |> Calendar.add_event c |> change true
    | Delete -> delete_instructions c |> change true
    | Edit -> edit_instructions c |> Calendar.edit_event c |> change true
    | Save -> Command.save_parse c; print_success (); change true c
    | View -> view_instructions c true
    | Exit -> print_exit_message (); exit 1
    | _ -> change false c
  with 
  | CannotAddExisting -> print_CannotAddExisting (); print_try_again (); change false c
  | EventDNE -> print_EventDNE (); print_try_again (); change false c

(** [start_cal file] loads json calendar at [file] and launches program with
    the calendar type representation. *)
let start_cal file = 
  let c = file |> Yojson.Basic.from_file |> Calendar.from_json in 
  change false c

(** [read_file ()] gets calendar name from user ands load calendars into program*)
let rec read_file () = 
  print_endline "Enter the calendar you would like to edit:\n";
  Stdlib.print_string  "> ";
  try
    match read_line () with
    | exception End_of_file -> ()
    | cal_name -> start_cal (cal_name ^ ".json")
  with 
    _ -> print_endline "\nThat calendar does not exist. Make sure you enter an existing calendar name!\n"; read_file ()

(** [meta_instructions ()] lists the highest level instructions *)
let rec meta_instructions () = 
  ANSITerminal.(print_string [red] "Would you like to create a new calendar or access an existing one?\n");
  ANSITerminal.(print_string [blue] "Enter 'create' or 'access'\n");
  Stdlib.print_string "Type here: > ";
  try 
    let meta = (Stdlib.read_line ()|> meta_parse) in 
    if meta = "access" then read_file ();
    if meta = "create" then let n = create_instructions () in 
      change true (Calendar.empty n)
  with 
  | MetaCommandDNE -> print_MetaCommandDNE ();
    meta_instructions ()

(** [main ()] starts the calendar interface *)

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Adaptive Calender system!\n");
  meta_instructions ()

(* Execute the calendar engine. *)

let () = main ()