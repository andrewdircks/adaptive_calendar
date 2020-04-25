
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

(**  [to_json c] is the JSON file that [c] represents. *)
val to_json :  t -> unit

(** [get_events c] is the list of events in calendar [c]. 
    val get_events : t -> event list*)

(** [todays_events d c] is the list of events in calendar [c] on date [d]. 
    val todays_events : t -> event list*)

(**[add_event e c] is [c] with event [e] added. 
    Raises: [CannotAddExisting] if a similar event exists. *)
val add_event : t -> event -> t

(** [delete_event (name,start) c] ic [c] with the event of name [name] and start
    time [start] deleted.
    Raises: [EventDNE] if there is no such event. *)
val delete_event : t -> (string * Time.t) -> t

(** [edit_event info c] is [c] but with the event of name and start time as in 
    [info] edited according to field and new value of [info].
    Requires: [info] is a tuple (oldname, oldstarttime, field, newvalue)
    Raises: [EventDNE] if there is no such event.*)
val edit_event : t -> (string * Time.t * string * string) -> t

(** [change_name n c] is [c] with name changed to [n]. 
    Requires: [n] is a non-empty string. *)
val change_name : string -> event -> event

(** [change_name d c] is [c] with description changed to [d]. *)
val change_description : string -> event -> event

(** [change_start_time st c] is [c] with start time changed to [st]. *)
val change_start_time : Time.t -> event -> event

(** [change_end_time et c] is [c] with end time changed to [ed]. *)
val change_end_time : Time.t -> event -> event

val parse_file : string -> t

