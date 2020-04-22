(** 
   Time functions and local time zone information of user.
*)

(** Type representing years. Valid range: 2020... *)
type year = int

(** Type representing months. *)
type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

(** Type representing days of the month. *)
type day_m = int

(** Type representing days of the week. *)
type day_w = Sun | Mon | Tue | Wed | Thu | Fri | Sat

(** Type representing time of minutes. We choose to represent times in 15 minute 
    intervals. *)
type time_m = T0 | T15 | T30 | T45

(** Type representing time of day. Valid range for hour: 0..23 *)
type time_d = {
  hour : int;
  minute : time_m;
}

(** Abstract type representing date and time. *)
type t = {
  year : year;
  month : month;
  day_m : day_m;
  day_w : day_w;
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

(** [toLocal d] is [d] represented in GMT.
    Requires: [d] represents a time in the users time zone. *)
val toGMT : t -> t

(** [is_leap_year] is true if the current year is a leap year, and false
    otherwise. *)
val is_leap_year : bool

(** [now] is the current date and time of the user. *)
val now : t

(** [from_string str] is the date and time value of [str].
    Requires: [str] is in the format "mm/dd/yyyy/hh:zz/xx": 
    where mm represents the month number, dd represents the day number,
    yyyy represents the year number, hh represents the hour number, zz represents
    the minute number, and xx is either "AM" or "PM".*)
val from_string : string -> t

(** [occurs_before d1 d2] is true if [d1] is earlier thatn [d2] and 
    false otherwise. *)
val occurs_before : t -> t -> bool