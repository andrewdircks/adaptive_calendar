open ANSITerminal
open Command 
open Calendar 

(* Raised if the user wants to return back to main edit page. *)
exception Back

(* Raised if the user wants to print in depth instructions. *)
exception Help

(** [validate_name n] ensures that user inputted name [n] is not empty.  *)
let validate_name n = if String.trim n = "" then raise EmptyEventName else n

(** [help_back str] checks that user input [str] is not the 'help' or 'back
    commands. If [str] is one of these commands, the respective errors are raised,
    otherwise, [str] is returned. *)
let help_back str = 
  if str = "back" then raise Back 
  else if str = "help" then raise Help 
  else str

(** [print_commands ()] prints the description of valid user commands. *)
let print_commands () : unit = 
  ANSITerminal.(print_string [blue] "Add: Make new event\n");
  ANSITerminal.(print_string [blue] "Edit: Modify existing event\n");
  ANSITerminal.(print_string [blue] "Delete: Delete existing event\n");
  ANSITerminal.(print_string [blue] "View: View events\n");
  ANSITerminal.(print_string [blue] "Save: Save your changes\n");
  ANSITerminal.(print_string [blue] "Exit: Exit the application (does not save)\n");
  ANSITerminal.(print_string [blue] "Help: Displays more detailed instructions. Can be called any time and progress maintainted.\n");
  ANSITerminal.(print_string [blue] "Back: Stops the current action and returns to this screen. \n \n")


(** [print_helps ()] prints the help instructions. *)
let print_help () : unit = 
  ANSITerminal.(print_string [green; Bold;] "\n\n\n");
  ANSITerminal.(print_string [green; Bold; on_black] "Help:");
  ANSITerminal.(print_string [black] "\n\nThis calendar application is written in OCaml by Andrew Dircks and Samuel Kantor.\n\n");
  ANSITerminal.(print_string [cyan; Bold] "Intended Use:\n");
  ANSITerminal.(print_string [black] "This calendar is designed for software development teams using Git.\nWith this source code in a git repository, team members can create and edit calendars,\nwhich can be accessed, viewed, and altered anywhere, automatically adaptive to time zone. \n\n");
  ANSITerminal.(print_string [blue; Bold] "Overview:\n");
  ANSITerminal.(print_string [black] "Upon running 'make cal', the user can input 'create' to make a new shared calendar or 'access' to view/edit an existing one.\n");
  ANSITerminal.(print_string [black] "Once a calendar is accessed, the user can input the following commands:\n \n");

  ANSITerminal.(print_string [cyan; Bold] "Add:\n");
  ANSITerminal.(print_string [black] "The add command is responsible for creating new events within the calendar.\n");
  ANSITerminal.(print_string [black] "Once called, the user is prompted to add a NONEMPTY event name. Make this short and descriptive, and avoid the names 'help' or 'back'.\n");
  ANSITerminal.(print_string [black] "Then, a start date is required. The form month/day/year/hour:minute/am(pm) is required.\n");
  ANSITerminal.(print_string [black] "The user will be told if an invalid format or illegal date is entered. Events before the day Jan 1, 2020 are not allowed.\n");

  ANSITerminal.(print_string [cyan; Bold] "\nEdit:\n");
  ANSITerminal.(print_string [black] "The edit command is responsible for editing existing events in the calendar.\n");
  ANSITerminal.(print_string [black] "Once called, the user is prompted to enter the name of the event they would like to edit. (case sensitive)\n");
  ANSITerminal.(print_string [black] "If this event does not exist, the user will be reprompted.\n");
  ANSITerminal.(print_string [black] "If multiple events are found with the same name, the application will display the start times of each,\n");
  ANSITerminal.(print_string [black] "and the user will be required to choose the entry to be edited.\n");
  ANSITerminal.(print_string [black] "The user must then enter 'name' 'start' 'end' or 'description' and update the respective field. \n");

  ANSITerminal.(print_string [cyan; Bold] "\nDelete:\n");
  ANSITerminal.(print_string [black] "The delete command is responsible for removing events in the calendar.\n");
  ANSITerminal.(print_string [black] "Once called, the user is prompted to enter the name of the event they would like to delete. (case sensitive)\n");
  ANSITerminal.(print_string [black] "If multiple events are found with the same name, the application will display the start times of each.\n");
  ANSITerminal.(print_string [black] "and the user will be required to choose the entry to be deleted.\n");

  ANSITerminal.(print_string [cyan; Bold] "\nView:\n");
  ANSITerminal.(print_string [black] "The view command is responsible for viewing your calendar!\n");
  ANSITerminal.(print_string [black] "Once called, the user has the choice for two viewing options: a single event, or a week.\n");
  ANSITerminal.(print_string [black] "If you would like to view a single event, simply enter the name of that event (case sensitive).\n");
  ANSITerminal.(print_string [black] "Similar to 'edit' and 'delete', the application handles the case of the event not existing and/or multiple events existing with that name.\n");
  ANSITerminal.(print_string [black] "If you would like to view a week, enter the START date of that week in the following formats: \n");
  ANSITerminal.(print_string [black; Bold] " 'dd' or 'mm/dd/' or 'mm/dd/yyyy'. \n");
  ANSITerminal.(print_string [black] "If only the day is entered (e.g. '5') then the week starting on the user's current year, month, with the entered day is shown. \n");
  ANSITerminal.(print_string [black] "If both the month and day are entered (e.g. '3/12') then the week starting on the user's current year, with the entered day and month is shown. \n");
  ANSITerminal.(print_string [black] "If all three fields are entered (e.g. '1/2/2020') then the week starting on that day is entered. \n");

  ANSITerminal.(print_string [cyan; Bold] "\nSave:\n");
  ANSITerminal.(print_string [black] "The save command saves the changes you have made to the current calendar.\n");
  ANSITerminal.(print_string [black] "Make sure you call this command before exiting, if you'd like to keep your changes!.\n");

  ANSITerminal.(print_string [cyan; Bold] "\nExit:\n");
  ANSITerminal.(print_string [black] "The exit command exits the application (does not save your progress).\n");

  ANSITerminal.(print_string [cyan; Bold] "\nBack:\n");
  ANSITerminal.(print_string [black] "The back command can be called at any time during application use.\n");
  ANSITerminal.(print_string [black] "Upon call, the user is brought back to the main instructions page.\n");
  ANSITerminal.(print_string [black] "Immedieate progress will be lost. For example, if a user calls 'back' while adding an event,.\n");
  ANSITerminal.(print_string [black] "the progress in that add command will be lost.\n\n\n")

(** [read] allows users to input data and checks for 'help' or 'back' keywords. *)
let read () = 
  try
    Stdlib.read_line () |> help_back
  with 
  | Back -> raise Back 
  | Help -> print_help (); raise Help

(** [print_success ()] notifies user of a succesful operation *)
let print_success () = 
  ANSITerminal.(print_string [red] "Success! \n")

(** [print_try_again ()] displays a try again message, to handle
    unexpected user inputs. *)
let print_try_again () = 
  ANSITerminal.(print_string [red] "Try again. \n")

(** [print_InvalidDate ()] displays invalid date error message. *)
let print_InvalidDate () = 
  ANSITerminal.(print_string [red] "The date you entered does not exist. \n\n")

(** [print_MetaCommandDNE ()] displays meta command does not exist error message. *)
let print_MetaCommandDNE () = 
  ANSITerminal.(print_string [red] "Enter 'create' or 'access'.\n")

(** [print_CommandDNE ()] displays command does not exist error message. *)
let print_CommandDNE () = 
  ANSITerminal.(print_string [red] "That command does not exist.\n")

(** [print_EventDNE ()] displays event does not exist error message. *)
let print_EventDNE () = 
  ANSITerminal.(print_string [red] "That event doesnt exist. Make sure you have the correct event name and start time.\n")

(** [print_invalidWeek ()] displays event invalid week error message. *)
let print_invalidWeek () = 
  ANSITerminal.(print_string [red] "Make sure the week you are trying to view is in the following forms: \n");
  ANSITerminal.(print_string [black; Bold] " 'dd' or 'mm/dd/' or 'mm/dd/yyyy'. \n");
  ANSITerminal.(print_string [black] "For 'dd', the week that starts at the entered day, the user's month, and the user's year is viewed. \n");
  ANSITerminal.(print_string [black] "For 'mm/dd' the week that starts at the entered month (mm) and day (dd) with the user's year is viewed. \n");
  ANSITerminal.(print_string [black] "For 'mm/dd/yyyy' then the week starting on that day is entered. \n")


(** [print_InvalidDateString ()] displays invalid date message. *)
let print_InvalidDateString () : unit = 
  ANSITerminal.(print_string [red; Bold] ("\nThe date you entered is not in the correct form.\n"));
  ANSITerminal.(print_string [green] ("Make sure you enter a date in the form 'mm/dd/yyyy/hh:ii/xx where:' \n" 
                                      ^  "mm is the month in range [01..12]\n"
                                      ^  "dd is the day in range [01..31]\n"
                                      ^  "yyyy is the year in range [2020..]\n"
                                      ^  "hh is the hour ins range [01..12]\n"
                                      ^  "ii is the minute in range [00..59]\n"
                                      ^  "xx is 'am' or 'pm'\n\n"))

(** [print_MalformedList ()] displays malformed list message *)
let print_MalformedList () = 
  ANSITerminal.(print_string [red] "This shouldnt happen.\n")

(** [print_CannotAddExisting ()] displays error when user inputs empty string *)
let print_CannotAddExisting () = 
  ANSITerminal.(print_string [red] "You cannot add an event with the same name and start date as another event. Try again. \n")

(** [print_EmptyEventName ()] displays error when user inputs empty string *)
let print_EmptyEventName () = 
  ANSITerminal.(print_string [red] "Enter a non empty name.\n")

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
  ANSITerminal.(print_string [blue] "Multiple events with this name exist. When does this event start? (enter the number as labeled). \n")

(** [main_instructions disp] is the main recursive function once a calendar is selected.
    It enables the user to run functions that interact with the calendar.*)
let rec main_instructions disp = 
  ANSITerminal.(print_string [Bold] "\nWhat would you like to do?\n");
  if disp then print_commands ();
  Stdlib.print_string "Type here: > ";
  try read () |> main_parse 
  with 
  | CommandDNE -> print_CommandDNE (); main_instructions false
  | Back -> main_instructions false
  | Help -> main_instructions false

(** [event_name_instructions disp] prompts user for name of event. *)
let rec event_name_instructions disp = 
  if disp then ANSITerminal.(print_string [red] "What is the name of this event?\n");
  Stdlib.print_string "Type here: > ";
  try
    read () |> validate_name
  with 
  | EmptyEventName -> print_EmptyEventName (); event_name_instructions false
  | Help -> event_name_instructions true

(** [vevent_or_weel_instructions ()] prompts user to enter an event name or week to view. *)
let rec event_or_week_instructions () = 
  ANSITerminal.(print_string [green] "Enter the name on the event you would like to view.\n");
  ANSITerminal.(print_string [green] "Or, enter the starting date of the week you would like to view.\n");
  Stdlib.print_string "Type here: > ";
  try read () with Help -> event_or_week_instructions ()

(** [event_delete_edit_name_instructions action] prompts user for event name to perform
    [action] on. *)
let rec event_delete_edit_name_instructions action = 
  ANSITerminal.(print_string [red] ("What is the name of the event to " ^ action ^ "?\n"));
  Stdlib.print_string "Type here: > ";
  try read () with Help -> event_delete_edit_name_instructions action

(** [start_time_instructions disp] prompts user for start time value. *)
let rec start_time_instructions disp : Time.t = 
  if disp then ANSITerminal.(print_string [red] "When does this event start?\n");
  Stdlib.print_string "Type here: > ";
  try
    let t =  read () |> Time.from_input_string 
    in if Time.is_valid t then t else raise InvalidDate
  with 
  | Help -> start_time_instructions true
  | InvalidDate -> 
    print_InvalidDate (); print_try_again (); start_time_instructions false
  | _ -> 
    print_InvalidDateString (); print_try_again (); start_time_instructions false

(** [end_time_instructions start disp] prompts user for end time value and 
    checks for valid time input. *)
let rec end_time_instructions start disp = 
  if disp then ANSITerminal.(print_string [red] "How long does this event last? (either enter a number of hours or hours:minutes)\n");
  Stdlib.print_string "Type here: > ";
  try 
    read () |> duration_parse start |> handle_date_input start
  with 
  | Help -> end_time_instructions start disp
  | StartAfterEnd -> 
    print_StartAfterEnd (); print_try_again (); end_time_instructions start false
  | InvalidDate -> 
    print_InvalidDate (); print_try_again (); end_time_instructions start false
  | _ -> 
    print_InvalidDateString (); print_try_again (); end_time_instructions start false

(** [description_instructions ()] prompts user for event's description. *)
let rec description_instructions () = 
  ANSITerminal.(print_string [red] "What's this event's description?\n");
  Stdlib.print_string "Type here: > ";
  try read () with Help -> description_instructions ()

(** [field_instructions ()] prompts user for field to change. *)
let rec field_instructions () = 
  ANSITerminal.(print_string [red] "What do you want to edit - name, start, end, or description?\n");
  Stdlib.print_string "Type here: > ";
  try read () with Help -> field_instructions ()

(** [field_edit_instructions field] prompts user for changed value of [field]*)
let rec field_edit_instructions field = 
  ANSITerminal.(print_string [red] ("What would you like to change the " ^ field ^ " to?\n"));
  Stdlib.print_string "Type here: > ";
  try read () with Help -> field_edit_instructions field

(** [print_exit_message ()] prints the exit message. *)
let print_exit_message () = 
  ANSITerminal.(print_string [green] ("Exiting calendar. See you soon!\n"))

(** [create_instructions ()] runs instructions for creating calendar*)
let rec create_instructions () : string =
  ANSITerminal.(print_string [red] ("What is the name of your new calendar? Make sure you do not choose a name of an existing calendar.\n"));
  Stdlib.print_string "Type here: > ";
  try
    read () |> Command.create_parse
  with 
  | Help -> create_instructions ()
  | EmptyCalendarName -> print_try_again (); (create_instructions ())

let rec print_start_times es acc : unit = 
  match es with 
  | [] -> ()
  | h::t -> ANSITerminal.(print_string [red] ((string_of_int acc) ^ ". " ^ (Graphic.to_display_time (h.starts |> Time.toLocal)) ^ "\n"));
    print_start_times t (acc + 1)

(** [multiple_events_instructions es display] runs instructions for handling events
    with the same name. *)
let rec multiple_events_instructions es display : Calendar.event = 
  if display then print_multiple_events ();
  print_start_times es 1; 
  Stdlib.print_string "Type here: > ";
  try 
    read () |> int_of_string |> Command.multiple_event_parse es 
  with 
  | Help -> multiple_events_instructions es display
  | OutOfBounds -> print_OutOfBounds (); multiple_events_instructions es false
  | _ -> print_try_again (); multiple_events_instructions es false

(** [add_instructions ()] runs instructions for adding events*)
let rec add_instructions () : Calendar.event = 
  let name = event_name_instructions true in 
  let starttime = start_time_instructions true in 
  let endtime = end_time_instructions starttime true in 
  let description = description_instructions () in
  (add_parse (name, starttime, endtime, description))

(** [handle_multiple_events c name origin] is the event that the user 
    wants to perform an action on in [c], as specified by event name [name]. 
    If multiple events with [name] exist in [c], then the user is prompted
    to choose which start time they are reffering to. *)
let rec handle_multiple_events c name origin : Calendar.event = 
  (match Calendar.find_event c name with 
   | Some es -> (match es with 
       | h::[] -> h 
       | h::t -> multiple_events_instructions es true
       | _ -> print_OutOfBounds (); handle_multiple_events c name origin)
   | None -> print_EventDNE (); handle_multiple_events c (event_delete_edit_name_instructions "delete") origin)

(** [add_instructions c] runs instructions for deleting an event. *)
and delete_instructions (c : Calendar.t) : Calendar.t = 
  try
    let name = event_delete_edit_name_instructions "delete" in 
    let event = handle_multiple_events c name "delete"
    in
    Calendar.delete_event c event
  with 
  | EventDNE -> print_EventDNE (); delete_instructions c

(** [edit_instructions ()] runs instructions for editing events*)
and edit_instructions c = 
  let name = event_delete_edit_name_instructions "edit" in 
  let event = handle_multiple_events c name "edit" in
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
and view_instructions c inst : unit = 
  let command = event_or_week_instructions () in 
  try 
    match view_parse c command with 
    | Single es -> 
      (match es with 
       | [] -> failwith "find event error"
       | h::[] -> Graphic.view_single h; change false c
       | _ -> multiple_events_instructions es true |> Graphic.view_single; change false c )
    | Week t -> Graphic.view_week c t; change false c 
  with
  | EventDNE -> print_EventDNE (); print_try_again (); view_instructions c false
  | InvalidWeek -> print_invalidWeek (); print_try_again (); view_instructions c false
  | InvalidDateString -> print_InvalidDateString (); print_try_again (); view_instructions c false
  | _ -> print_try_again (); view_instructions c false

(** [change success c] is the primary recursive function for playing this application.*) 
and change (success : bool) (c : Calendar.t) : unit = 
  if success then print_success ();
  try
    match main_instructions true with  
    | Create -> let n = create_instructions () in change true (Calendar.empty n)
    | Add -> add_instructions () |> Calendar.add_event c |> change true
    | Delete -> delete_instructions c |> change true
    | Edit -> edit_instructions c |> Calendar.edit_event c |> change true
    | Save -> Command.save_parse c; print_success (); change false c
    | View -> view_instructions c true
    | Exit -> print_exit_message (); exit 1
  with 
  | CannotAddExisting -> print_CannotAddExisting (); print_try_again (); change false c
  | EventDNE -> print_EventDNE (); print_try_again (); change false c
  | Back -> change false c

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

(** [main ()] starts the calendar application interface *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Adaptive Calender system!\n");
  meta_instructions ()

(* Execute the calendar engine. *)
let () = main ()