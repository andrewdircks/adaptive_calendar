(** 
   Local time zone information of user.
*)

(** [difference] is the number of hours difference between this machine's 
    timezone and UTC. If [difference] is positive, the current time zone is 
    "ahead" (i.e. in the western hemisphere), if negative, the current time zone
    is "behind", and if zero, the current time zone is UTC. *)
val difference : int

(** [today] is the tuple representing the year, month, day of week, and day of 
    month, in that order, in the user's time zone. Months are in the range
    [0..11], days of the week are in the range [0..6], days of the month 
    are in range [1..31].*)
val today : int * int * int * int

(** [is_leap_year] is true if the current year is a leap year, and false
    otherwise. *)
val is_leap_year : bool