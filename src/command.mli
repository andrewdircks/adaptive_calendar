(** 
   Parsing of user commands.
*)

(** The type representing a calendar date. *)
type date_phrase = string

(** The type representing main user commands. *)
type command = 
  | Add 
  | Delete 
  | Edit 
  | View
  | Previous
  | Next
  | Save

(** The type representing [Add] commands.*)
type add_command 

(** The type representing [Delete] commands. *)
type delete_command 

(** The type representing [Delete] commands. *)
type edit_command 

(** [is_valid_date d] is true if [d] represents a valid calendar date and 
    false otherwise. *)
val is_valid_date : date_phrase -> bool
