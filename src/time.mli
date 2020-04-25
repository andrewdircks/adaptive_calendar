(** 
   Time functions and local time zone information of user.
*)

(** Type representing years. Valid range: 2020... *)
type year = int

(** Type representing months. *)
type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

(** Type representing days of the month. *)
type day_m = int

(** Type representing time of day. Valid range for hour: 0..23 *)
type time_d = {
  hour : int;
  minute : int;
}

(** Abstract type representing date and time. *)
type t = {
  year : year;
  month : month;
  day_m : day_m;
  time_d : time_d;
}

(** [is_valid d] is true if [d] is a valid date and time. *)
val is_valid : t -> bool

(** [difference] is the number of hours difference between this machine's 
    timezone and UTC. If [difference] is positive, the current time zone is 
    "ahead" (i.e. in the western hemisphere), if negative, the current time zone
    is "behind", and if zero, the current time zone is UTC. *)
val difference : int

(** [toLocal d] is [d] represented in the user's time zone.
    Requires: [d] represents a time in standard GMT time. *)
val toLocal : t -> t

(** [toGMT d] is [d] represented in GMT.
    Requires: [d] represents a time in the users time zone. *)
val toGMT : t -> t

(** [is_leap_year] is true if the current year is a leap year, and false
    otherwise. *)
val is_leap_year : bool

(** [now] is the current date and time of the user. *)
val now : t

(** [from_json_string str] is the date and time value of [str], as stored in json 
    files. Note, the time is in military time.
    Requires: [str] is in the format "mm/dd/yyyy/hh:zz/0": 
    where mm represents the month number, dd represents the day number,
    yyyy represents the year number, hh represents the hour number, zz represents
    the minute number.*)
val from_json_string : string -> t

(** [from_input_string str] is the date and time value of a [str] collected from user.
    Requires: [str] is in the format "mm/dd/yyyy/hh:zz/xx": 
    where mm represents the month number, dd represents the day number,
    yyyy represents the year number, hh represents the hour number, zz represents
    the minute number, and xx is am or pm.*)
val from_input_string : string -> t

(**
    [time_to_string tm] is the time value as a 24-hr string.
*)
val time_to_string : t -> string

(** [occurs_before d1 d2] is true if [d1] is earlier than [d2] and 
    false otherwise. *)
val occurs_before : t -> t -> bool

(** [compare_time t1 t2] implements the standard comparison function for times. *)
val compare_time : t -> t -> int