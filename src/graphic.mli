(** 
   Terminal viewing of calendars.
*)

(** [view_event e] displays [e] in terminal. *)
val view_event : Calendar.event -> unit 

(** [view_event c s] displays the events in [c] in the week that starts 
    at [s]. *)
val view_week : Calendar.t -> Time.t -> unit