(** 
   The main entry point for the calendar interface.
*)

(** Raised if the user wants to print in depth instructions. *)
exception Help

(** Raised if the user wants to return back to main edit page. *)
exception Back

(** [main ()] starts the calendar application interface *)
val main : unit -> unit