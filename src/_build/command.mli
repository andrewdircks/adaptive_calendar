(** 
   Parsing of user commands.
*)

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

(** The type representing main user commands. *)
type command = 
  | Add 
  | Delete 
  | Edit 
  | View
  | Previous
  | Next
  | Save

(** [main_parse str] parses a player's input into a command. Any capitilizations 
    of valid commands (i.e. words in type [command]) are valid.
    Raises: [CommandDNE] if an invalid command is entered. *)
val main_parse : string -> command

(** [add_parse str] parses a player's input into an event to be added to the 
    current calendar. 
    Requires: [str] is a list representing the title, starting date in 
    correct form, ending date in correct form, and description in that order.
    Raises: [InvalidDate] if an invalid date is entered. 
            [EmptyEventName] is no event name is entered. 
            [InvalidDateString] if the inputted date strings are not in correct form.
            [StartAfterEnd] if the start date is after the end date. 
            [MalformedList] if [str] is not a list of length four. *)
val add_parse : string list -> Calendar.event

(** [delete_parse str] parses a player's input into an event title and start date
    tuple representing the event to be deleted from the current calendar.
    Requires: [str] is a list representing the title and starting date in 
              that order.
    Raises: [InvalidDate] if an invalid date is entered. 
            [MalformedList] if [str] is not a list of length two. 
            [EmptyEventName] if no event name is entered. 
            [InvalidDateString] if the inputted date string is not in correct form.*)
val delete_parse : string list -> (string * Time.t)

(** [edit_parse str] is a tuple (oldname, oldstarttime, field, change) in which
    "oldname" represents the name of the event to be edited, "oldstarttime" 
    represents the start time of the event to be edited, "field" is the field
    to be changed in the event, and "change" is the new respective value in
    that field.
    Requires: [str] is a list as such: 
              [old name; old start date; field to edit; new value].
    Raises: [InvalidDate] if an invalid date is entered. 
            [MalformedList] if [str] is not a list of length 4. 
            [EmptyEventName] if no event name is entered. 
            [InvalidDateString] if the inputted date string is not in correct form.*)
val edit_parse : string list -> (string * Time.t * string * string)

