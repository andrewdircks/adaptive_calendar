(* open Yojson.Basic *)
open Time
(** 
   Representation of calendar data.

   This module represents the data stored in calendar files, including
   events and dates.  It handles loading of that data from JSON, writing data to 
   JSON, and updating event data.

   All times are handled in GMT.
*)

(** The abstract type of values representing calendars. *)
type t

(** The abstract type of values representing events. *)
type event

(** Raised when an unknown event is attempted to be accessed. *)
exception EventDNE

(** [from_json j] is the calendar that [j] represents.
    Requires: [j] is a valid JSON calendar representation. *)
val from_json : int -> t

(** [to_json c] is the JSON file that [c] represents. *)
val to_json :  t -> int

(** [get_events c] is the list of events in calendar [c]. *)
val get_events : t -> event list

(** [todays_events d c] is the list of events in calendar [c] on date [d]. *)
val todays_events : t -> event list

(** [add_event e c] adds event [e] to [c]. *)
val add_event : event -> t -> event list

(** [delete_event e c] removes event [e] from [c]. *)
val delete_event : event -> t -> event list

(** [replace_event e1 e2 c] adds event [e1] and removes event [e2] from [c]. *)
val replace_event : event -> event -> t -> event list

(** [event_from_name_date n d c] is the event . *)
val event_from_name_date : string -> Time.t -> t -> event

