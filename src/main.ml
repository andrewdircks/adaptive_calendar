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

(** [main_instructions ()] is the main recursive function once a calendar is selected.
    It enables the user to run functions that interact with the calendar.*)
let rec main_instructions () = 
  ANSITerminal.(print_string [red] "What would you like to do?\n");
  ANSITerminal.(print_string [blue] "Add: Make new event\n");
  ANSITerminal.(print_string [blue] "Edit: Modify existing event\n");
  ANSITerminal.(print_string [blue] "Delete: Delete existing event\n");
  ANSITerminal.(print_string [blue] "Save: Close and Save to file\n");

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

(** [view_instructions ()] prompts user to enter an event name or week to view. *)
let event_or_week_instructions () = 
  ANSITerminal.(print_string [red] "Enter an event name or week to view (in format mm/dd) \n");
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

(** [create_instructions ()] runs instructions for creating calendar*)
let rec create_instructions () : string =
  ANSITerminal.(print_string [red] ("What is the name of your new calendar? Make sure you do not choose a name of an existing calendar.\n"));
  Stdlib.print_string "Type here: > ";
  try
    Stdlib.read_line () |> Command.create_parse
  with 
  | EmptyCalendarName -> print_try_again (); (create_instructions ())

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

(** [delete_instructions ()] runs instructions for deleting events*)
let rec delete_instructions () = 
  let name = event_delete_edit_name_instructions "delete" in 
  let starttime = start_time_instructions () in 
  try delete_parse [name; starttime] 
  with 
  | InvalidDate -> print_InvalidDate (); delete_instructions ()
  | MalformedList -> print_MalformedList (); delete_instructions ()
  | EmptyEventName -> print_EmptyEventName (); delete_instructions ()
  | StartAfterEnd -> print_StartAfterEnd (); delete_instructions ()
  | InvalidDateString -> print_InvalidDateString (); delete_instructions ()
  | EventDNE -> print_EventDNE (); delete_instructions ()

(** [edit_instructions ()] runs instructions for editing events*)
let rec edit_instructions () = 
  let name = event_delete_edit_name_instructions "edit" in 
  let starttime = start_time_instructions () in 
  let field = field_instructions () in 
  let thechange = field_edit_instructions field in 
  try edit_parse [name; starttime; field; thechange] 
  with 
  | InvalidDate -> print_InvalidDate (); print_try_again (); edit_instructions ()
  | MalformedList -> print_MalformedList (); print_try_again (); edit_instructions ()
  | EmptyEventName -> print_EmptyEventName (); print_try_again (); edit_instructions ()
  | StartAfterEnd -> print_StartAfterEnd (); print_try_again (); edit_instructions ()
  | InvalidDateString -> print_InvalidDateString (); print_try_again (); edit_instructions ()
  | EventDNE -> print_EventDNE (); print_try_again (); edit_instructions ()

(** [view_instructions c] runs instructions for viewing events of [c]*)
let rec view_instructions c : unit = 
  let command = event_or_week_instructions () in 
  try 
    match view_parse c command with 
    | Single e -> Graphic.view_event e
    | Week _ -> failwith "havent implemented week view"
  with
  | _ -> print_try_again (); view_instructions c

(** [change success c] is the primary recursive function for playing this application.*) 
let rec change (success : bool) (c : Calendar.t) : unit = 
  if success then print_success ();
  try
    match main_instructions () with  
    | Create -> let n = create_instructions () in change true (Calendar.empty n)
    | Add -> add_instructions () |> Calendar.add_event c |> change true
    | Delete -> delete_instructions () |> Calendar.delete_event c |> change true
    | Edit -> edit_instructions () |> Calendar.edit_event c |> change true
    | Save -> Command.save_parse c; print_success (); exit 0
    | View -> view_instructions c
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
let read_file () = 
  print_endline "Enter the calendar you would like to edit:\n";
  Stdlib.print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | cal_name -> start_cal (cal_name ^ ".json")

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