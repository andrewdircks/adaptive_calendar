(** 
   Parsing of user commands.
*)

(** Raised if an invalid meta command is entered. *)
exception MetaCommandDNE

(** Raised if an invalid command is entered. *)
exception CommandDNE

(** Raised if an invalid date is entered. *)
exception InvalidDate

(** Raised if an arry of undesired length is entered. *)
exception MalformedList

(** Raised if an empty event name is entered. *)
exception EmptyEventName

(** Raised if an event is 0 minutes or if it starts before it ends. *)
exception StartAfterEnd

(** Raised if a date string is not in the proper form. *)
exception InvalidDateString

(** Raised if an invalid field to be edited is inputted. *)
exception InvalidField

(** Raised if an empty calendar name is entered. *)
exception EmptyCalendarName

(** Raised if an out of bounds index is called for a list. *)
exception OutOfBounds

(** Raised if an invalid duration is entered. *)
exception InvalidDuration

(** Raised if an invalid view week is entered. *)
exception InvalidWeek

(** The type representing meta user commands to start the application. *)
type view_option = Single of Calendar.event list | Week of Time.t

(** The type representing meta user commands to start the application. *)
type meta_command = Create | Access

(** The type representing main user commands. *)
type command = 
  | Create
  | Add 
  | Delete 
  | Edit 
  | View
  | Save
  | Exit

(** [main_parse str] parses a player's input into a command. Any capitilizations 
    of valid commands (i.e. words in type [command]) are valid.
    Raises: [CommandDNE] if an invalid command is entered. *)
val main_parse : string -> command

(** [meta_parse str] handles a player's initial to command, to either create a 
    new calendar or edit an existing one. 
    Raises: [MetaCommandDNE] if an invalid command is entered. *)
val meta_parse : string -> string

(** [add_parse str] parses a player's input into an event to be added to the 
    current calendar. 
    Raises: [InvalidDate] if an invalid date is entered. 
            [EmptyEventName] is no event name is entered. 
            [InvalidDateString] if the inputted date strings are not in correct form.
            [StartAfterEnd] if the start date is after the end date. 
            [MalformedList] if [str] is not a list of length four. *)
val add_parse : (string * Time.t * Time.t * string) -> Calendar.event

(** [handle_date_input t1 t2] is t2 if t2 occurs after t2. Otherwise,
    raises [StartAfterEnd].*)
val handle_date_input : Time.t -> Time.t -> Time.t 

(** [duration_parse t dur] ensures [dur] is a hour/minute input and 
    is [t] plus [dur] hours.*)
val duration_parse : Time.t -> string -> Time.t 

(** [delete_parse str] parses a player's input into an event title and start date
    tuple representing the event to be deleted from the current calendar.
    Requires: [str] is a list representing the title and starting date in 
              that order.
    Raises: [InvalidDate] if an invalid date is entered. 
            [MalformedList] if [str] is not a list of length two. 
            [EmptyEventName] if no event name is entered. 
            [InvalidDateString] if the inputted date string is not in correct form.*)
val delete_parse : string list -> (string * Time.t)

(** [edit_parse input] is a tuple (event, field, change) in which
    "event" is the event to be changed, "field" is the field
    to be changed in the event, and "change" is the new respective value in
    that field.
    Requires: [tuple] as such: 
              (event, field, change).
    Raises: [InvalidDate] if an invalid date is entered. 
            [MalformedList] if [str] is not a list of length 4. 
            [EmptyEventName] if no event name is entered. 
            [InvalidDateString] if the inputted date string is not in correct form.*)
val edit_parse : (Calendar.event * string * string) -> (Calendar.event * string * string)

(** [save_parse] writes the current calendar as a json file and returns unit. *)
val save_parse : Calendar.t -> unit

(** [create_parse name] creates a new calendar with name [name]a s a json file 
    [name].json and returns [name]. *)
val create_parse : string -> string

(** [view_parse name] returns a view option with the corresponding start date or 
    event. *)
val view_parse : Calendar.t -> string -> view_option

(** [multiple_event_parse es idx] is the event in [es] that has index [idx - 1]. 
    Raises: [OutOfBounds] if the start time of the event is not found. *)
val multiple_event_parse : Calendar.event list -> int -> Calendar.event
