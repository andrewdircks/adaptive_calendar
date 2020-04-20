(** 
   Represents the users current mode.
*)

(** The type representing a users mode. [Main] is the default mode in which 
    calendars can be altered, and [View] is the mode in which calendars are 
    graphically displayed. *)
type t = Main | View

(** Raised if the attempted action is attempted in an inappropriate mode. *)
exception InvalidMode

(** [is_valid_action cmd t] is the unit value if the correct mode is entered.
    Raises: [InvalidMode] if the attempted command is not valid. *)
val is_valid_action : Command.command -> t -> ()

