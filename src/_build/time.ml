open Unix

(** Type representing years. Valid range: 2020.. *)
type year = int

(** Type representing months. *)
type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

(** Type representing days of the month. Valid range: 1..31*)
type day_m = int

(** Type representing days of the week. *)
type day_w = Sun | Mon | Tue | Wed | Thu | Fri | Sat

(** Type representing time of minutes. We only represent times in 15 minute 
    intervals. *)
type time_m = T0 | T15 | T30 | T45

(** Type representing time of day. Valid range for hour: 0..23 *)
type time_d = {
  hour : int;
  minute : time_m;
}

type t = {
  year : year;
  month : month;
  day_m : day_m;
  day_w : day_w;
  time_d : time_d;
}

let is_leap_year = 
  let y = (Unix.time() |> Unix.localtime).tm_year + 1900 in 
  y mod 4 = 0 && (if y mod 100 = 0 then y mod 400 = 0 else true)

(** [is_valid_day_m dm m] is true if [dm] is a valid day number in month [m], 
    and false otherwise. *)
let is_valid_day_m (dm : day_m) (m : month) : bool = 
  match m with 
  | Feb ->  if is_leap_year then (dm >= 1 && dm <= 29) else (dm >= 1 && dm <= 28)
  | Jan | Mar | May | Jul | Aug | Oct | Dec -> (dm >= 1 && dm <= 31)
  | _ -> (dm >= 1 && dm <= 30)

let is_valid d = 
  let y = (d.year >= 2020) in 
  let dm = is_valid_day_m d.day_m d.month in 
  let td = (d.time_d.hour >= 0 && d.time_d.hour < 24) in 
  y && dm && td

(** [is_ahead_year local gm] is true if the [local] year is "ahead" of the [gm] 
    day.*)
let is_ahead_year (local : Unix.tm) (gm : Unix.tm) = 
  if local.tm_year - gm.tm_year > 0 then true else false

(** [is_ahead_day local gm] is true if the [local] day is "ahead" of the [gm] 
    day.*)
let is_ahead_day (local : Unix.tm) (gm : Unix.tm) = 
  if local.tm_year - gm.tm_year <> 0 then is_ahead_year local gm 
  else if local.tm_yday - gm.tm_yday > 0 then true else false

(** [is_ahead local gm] is true if [local] time is "ahead" of [gm] time, i.e. 
    there is a positive time zone difference. *)
let is_ahead (local : Unix.tm) (gm : Unix.tm) = 
  if local.tm_yday - gm.tm_yday <> 0 then is_ahead_day local gm 
  else if local.tm_hour - gm.tm_hour > 0 then true else false

let difference = 
  let t = Unix.time() in
  let local = Unix.localtime t in
  let gm = Unix.gmtime t in 
  if is_ahead local gm then 
    (if is_ahead_day local gm then local.tm_hour + (24 - gm.tm_hour) 
     else local.tm_hour - gm.tm_hour)
  else 
    (if is_ahead_day gm local then gm.tm_hour + (24 - local.tm_hour) 
     else local.tm_hour - gm.tm_hour)

(** [increment_month d] is [d] with its month incremented and day of month 
    set to 1. *)
let increment_month (d : t) : t = 
  let m' = 
    match d.month with 
    | Jan -> Feb
    | Feb -> Mar
    | Mar -> Apr
    | Apr -> May
    | May -> Jun
    | Jun -> Jul
    | Jul -> Aug
    | Aug -> Sep
    | Sep -> Oct
    | Oct -> Nov 
    | Nov -> Dec 
    | Dec -> Jan
  in 
  match m' with 
  (* handle case where incrementing month increments the year. *)
  | Jan -> 
    { year = d.year + 1;
      month = m';
      day_m = 1;
      day_w = d.day_w;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  | _ -> 
    { year = d.year;
      month = m';
      day_m = 1;
      day_w = d.day_w;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }

(** [decrement_month d] is [d] with its month decremented and day of month 
    set to an appropriate value. *)
let decrement_month (d : t) : t = 
  let m' = 
    match d.month with 
    | Jan -> Dec
    | Feb -> Jan
    | Mar -> Feb
    | Apr -> Mar
    | May -> Apr
    | Jun -> May
    | Jul -> Jun
    | Aug -> Jul
    | Sep -> Aug
    | Oct -> Sep 
    | Nov -> Oct 
    | Dec -> Nov
  in 
  match m' with 
  (* handle case where you are going back to February. *)
  | Feb -> 
    if is_leap_year then
      { year = d.year;
        month = m';
        day_m = 29;
        day_w = d.day_w;
        time_d = {
          hour = d.time_d.hour;
          minute = d.time_d.minute;
        } }
    else 
      { year = d.year;
        month = m';
        day_m = 28;
        day_w = d.day_w;
        time_d = {
          hour = d.time_d.hour;
          minute = d.time_d.minute;
        } }
  (* handle case where you are going back to a month with 31 days, not Dec. *)
  | Jan | Mar | May | Jul | Aug | Oct ->  
    { year = d.year;
      month = m';
      day_m = 31;
      day_w = d.day_w;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  (* handle case where you are going back a year. *)
  | Dec -> 
    { year = d.year - 1;
      month = m';
      day_m = 31;
      day_w = d.day_w;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  (* handle case where you are going back to a month with 30 days. *)
  | _ -> 
    { year = d.year;
      month = m';
      day_m = 30;
      day_w = d.day_w;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }

(** [increment_day d] is [d] with its days of the month and week incremented. *)
let increment_day (d : t) : t = 
  let dw' = 
    match d.day_w with 
    | Sun -> Mon
    | Mon -> Tue
    | Tue -> Wed
    | Wed -> Thu
    | Thu -> Fri
    | Fri -> Sat
    | Sat -> Sun
  in 
  (* increase day of month and week *)
  let d' = 
    { year = d.year;
      month = d.month;
      day_m = d.day_m + 1;
      day_w = dw';
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  in 
  (* ensure day of month is valid *)
  if is_valid d' then d' else increment_month d'

(** [decrement_day d] is [d] with its days of the month and week decremented. *)
let decrement_day (d : t) : t = 
  let dw' = 
    match d.day_w with 
    | Sun -> Sat
    | Mon -> Sun
    | Tue -> Mon
    | Wed -> Tue
    | Thu -> Wed
    | Fri -> Thu
    | Sat -> Fri
  in 
  (* decrease day of month and week *)
  let d' = 
    { year = d.year;
      month = d.month;
      day_m = d.day_m - 1;
      day_w = dw';
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  in 
  (* ensure day of month is valid *)
  if is_valid d' then d' else decrement_month d'


(** [increment_hour d dt] is [d] with its hour incremented by [dt] hours. 
    Requires: [dt] is a positive integer less than 24. *)
let increment_hour (d : t) (dt : int) : t = 
  let h' = d.time_d.hour + dt in 
  (* handle going into the next day *)
  if h' > 23 then 
    increment_day { year = d.year;
                    month = d.month;
                    day_m = d.day_m;
                    day_w = d.day_w;
                    time_d = {
                      hour = h'-24;
                      minute = d.time_d.minute;
                    } } 

  (* handle going into the previous day *)
  else if h' < 0 then
    decrement_day { year = d.year;
                    month = d.month;
                    day_m = d.day_m;
                    day_w = d.day_w;
                    time_d = {
                      hour = d.time_d.hour + 23;
                      minute = d.time_d.minute;
                    } } 

  (* stayed in the current day *)              
  else { year = d.year;
         month = d.month;
         day_m = d.day_m;
         day_w = d.day_w;
         time_d = {
           hour = h';
           minute = d.time_d.minute;
         } } 


let toLocal d = increment_hour d difference

let toGMT d = increment_hour d (-difference)

(** [month_from_int m] the [month] analog of [m].
    Requires: [m] is between 0 and 11.*)
let month_from_int (m : int) : month = 
  if m = 0 then Jan
  else if m = 1 then Feb
  else if m = 2 then Mar 
  else if m = 3 then Apr 
  else if m = 4 then May 
  else if m = 5 then Jun 
  else if m = 6 then Jul
  else if m = 7 then Aug 
  else if m = 8 then Sep 
  else if m = 9 then Oct 
  else if m = 10 then Nov 
  else Dec

(** [dayw_from_int d] the [day_w] analog of [d].
    Requires: [d] is between 0 and 6. *)
let dayw_from_int (d : int) : day_w = 
  if d = 0 then Sun
  else if d = 1 then Mon
  else if d = 2 then Tue
  else if d = 3 then Wed
  else if d = 4 then Thu 
  else if d = 5 then Fri
  else Sat

(** [min_to_quarters m] the [time_m] analog of [m], with rounding down.
    Requires: [m] is between 0 and 60. *)
let min_to_quarters (m : int) : time_m = 
  if m < 15 then T0 
  else if m < 30 then T15 
  else if m < 45 then T30 
  else T45

let now =
  let tm = Unix.localtime (Unix.time()) in 
  { year = tm.tm_year + 1900;
    month = month_from_int tm.tm_mon;
    day_m = tm.tm_mday;
    day_w = dayw_from_int tm.tm_wday;
    time_d = {
      hour = tm.tm_hour;
      minute = min_to_quarters tm.tm_min;
    } } 