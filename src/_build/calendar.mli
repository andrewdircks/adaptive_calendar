
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

(** [from_json j] is the calendar that [j] represents.
    Requires: [j] is a valid JSON calendar representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json c] is the JSON file that [c] represents. 
val to_json :  t -> Yojson.Basic.t*)

(** [get_events c] is the list of events in calendar [c]. 
val get_events : t -> event list*)

(** [todays_events d c] is the list of events in calendar [c] on date [d]. 
val todays_events : t -> event list*)

(** [add_event e c] adds event [e] to [c]. 
val add_event : event -> t *)

(** [delete_event (name,start) c] removes the event with name [name] and start 
    time [start] from [c].
    Raises: [EventDNE] if there is no such event. 
val delete_event : (string * Time.t) -> t *)

(** [replace_event e1 e2 c] adds event [e1] and removes event [e2] from [c]. 
val replace_event : event -> event -> t*)

(** [event_from_name_date n d c] is the event . 
val event_from_name_date : string -> Time.t -> t -> event*)

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

