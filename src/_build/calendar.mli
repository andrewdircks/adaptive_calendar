
(** 
   Representation of calendar data.

   This module represents the data stored in calendar files, including
   events and dates.  It handles loading of that data from JSON, writing data to 
   JSON, and updating event data.

   All times are handled in GMT.
*)

(** The abstract type of values representing events. *)
type event =  {
  starts : Time.t;
  ends : Time.t;
  name : string;
  description : string;
}

(** The abstract type of values representing calendars. *)
type t = {
  calname: string;
  events: event list;
}

(** Raised when an unknown event is attempted to be accessed. *)
exception EventDNE

(** Raised if an existing event with same name and start time is attempted 
    to be added to this calendar. *)
exception CannotAddExisting

(** Raised when a tuple of undesirable form is entered. *)
exception MalformedTuple

(** [empty n] is a calendar with calname [n] and no events. *)
val empty : string -> t

(** [from_json j] is the calendar that [j] represents.
    Requires: [j] is a valid JSON calendar representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json c] is the JSON file that calendar [c] represents. *)
val to_json :  t -> unit

(**[find_event c str] is Some [e] if [e] is a list of events in [c] with nanme 
   [str] and None otherwise.  *)
val find_event : t -> string -> (event list) option

(**[add_event e c] is calendar [c] with event [e] added. 
    Raises: [CannotAddExisting] if a similar event exists. *)
val add_event : t -> event -> t

(** [delete_event e c] ic [c] with [e] deleted
    Raises: [EventDNE] if there is no such event. *)
val delete_event : t -> event -> t

(** [edit_event info c] is [c] but with the event in [info] edited according 
    to field and new value of [info].
    Requires: [info] is a tuple (event field, newvalue)
    Raises: [EventDNE] if there is no such event.*)
val edit_event : t -> (event * string * string) -> t

(** [change_name n e] is event [e] with name changed to [e]. 
    Requires: [n] is a non-empty string. *)
val change_name : string -> event -> event

(** [change_description d e] is event [e] with description changed to [d]. *)
val change_description : string -> event -> event

(** [change_start_time st e] is [e] with start time changed to [st]. *)
val change_start_time : Time.t -> event -> event

(** [change_end_time et e] is event [e] with end time changed to [et]. *)
val change_end_time : Time.t -> event -> event

(** [parse_file fname] is the calendar [c] stored in the json file [fname]
    Requires: [fname] is a valid json file corresponding to a calendar.*)
val parse_file : string -> t

(** [get_week t c] is the list of events in [c] that occur on the day of [t] and 
    six days after. *)
val get_week : Time.t -> t -> event list

val sort_events: event list -> event list


