open Unix

(** Type representing years. Valid range: 2020.. *)
type year = int

(** Type representing months. *)
type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

(** Type representing days of the month. Valid range: 1..31*)
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

let difference = 
  let now = Unix.time () in 
  let gmt = fst (Unix.mktime (Unix.gmtime now)) in
  let local = fst (Unix.mktime (Unix.localtime now))
  in ((local -. gmt) /. (60. *. 60.)) |> Float.to_int

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
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  | _ -> 
    { year = d.year;
      month = m';
      day_m = 1;
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
        time_d = {
          hour = d.time_d.hour;
          minute = d.time_d.minute;
        } }
    else 
      { year = d.year;
        month = m';
        day_m = 28;
        time_d = {
          hour = d.time_d.hour;
          minute = d.time_d.minute;
        } }
  (* handle case where you are going back to a month with 31 days, not Dec. *)
  | Jan | Mar | May | Jul | Aug | Oct ->  
    { year = d.year;
      month = m';
      day_m = 31;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  (* handle case where you are going back a year. *)
  | Dec -> 
    { year = d.year - 1;
      month = m';
      day_m = 31;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  (* handle case where you are going back to a month with 30 days. *)
  | _ -> 
    { year = d.year;
      month = m';
      day_m = 30;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }

(** [increment_day d] is [d] with its days of the month and week incremented. *)
let increment_day (d : t) : t = 
  let d' = 
    { year = d.year;
      month = d.month;
      day_m = d.day_m + 1;
      time_d = {
        hour = d.time_d.hour;
        minute = d.time_d.minute;
      } }
  in 
  (* ensure day of month is valid *)
  if is_valid d' then d' else increment_month d'

(** [decrement_day d] is [d] with its days of the month and week decremented. *)
let decrement_day (d : t) : t = 
  (* decrease day of month and week *)
  let d' = 
    { year = d.year;
      month = d.month;
      day_m = d.day_m - 1;
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
                    time_d = {
                      hour = h'-24;
                      minute = d.time_d.minute;
                    } } 

  (* handle going into the previous day *)
  else if h' < 0 then
    decrement_day { year = d.year;
                    month = d.month;
                    day_m = d.day_m;
                    time_d = {
                      hour = d.time_d.hour + 23;
                      minute = d.time_d.minute;
                    } } 

  (* stayed in the current day *)              
  else { year = d.year;
         month = d.month;
         day_m = d.day_m;
         time_d = {
           hour = h';
           minute = d.time_d.minute;
         } } 


let toLocal d = increment_hour d difference

let toGMT d = increment_hour d (-difference)

(** [month_from_int m] the [month] analog of [m].
    Requires: [m] is between 1 and 12.*)
let month_from_int (m : int) : month = 
  if m = 1 then Jan
  else if m = 2 then Feb
  else if m = 3 then Mar 
  else if m = 4 then Apr 
  else if m = 5 then May 
  else if m = 6 then Jun 
  else if m = 7 then Jul
  else if m = 8 then Aug 
  else if m = 9 then Sep 
  else if m = 10 then Oct 
  else if m = 11 then Nov 
  else Dec


let now =
  let tm = Unix.localtime (Unix.time()) in 
  { year = tm.tm_year + 1900;
    month = month_from_int (tm.tm_mon+1);
    day_m = tm.tm_mday;
    time_d = {
      hour = tm.tm_hour;
      minute = tm.tm_min;
    } } 


let from_json_string (str:string) = 
  if String.length str <> 16 then failwith ("invalid string length: " ^ str) else 
    {
      year = String.sub str 6 4 |> int_of_string;
      month = String.sub str 0 2 |> int_of_string |> month_from_int;
      day_m = String.sub str 3 2 |> int_of_string;
      time_d = {
        hour = String.sub str 11 2 |> int_of_string ;
        minute = String.sub str 14 2 |> int_of_string ;
      }
    }

(** [to_military ampm hour] is the hr [hour]
    of string type [ampm] into 24-hr military time.*)
let to_military ampm hour = 
  let ap = String.lowercase_ascii ampm in
  if ap = "am" then 
    if hour = 12 then 0
    else hour
  else if ap = "pm" then 
    if hour != 12 then hour + 12
    else 12
  else failwith ("not am or pm")


let from_input_string (str:string) = 
  if String.length str <> 19 then failwith ("invalid string length: " ^ str) else 
    {
      year = String.sub str 6 4 |> int_of_string;
      month = String.sub str 0 2 |> int_of_string |> month_from_int;
      day_m = String.sub str 3 2 |> int_of_string;
      time_d = {
        hour = String.sub str 11 2 |> int_of_string |> to_military (String.sub str 17 2);
        minute = String.sub str 14 2 |> int_of_string ;
      }
    }

(** [month_to_int m] is the integer representation of [m]. *)
let month_to_int m = 
  match m with 
  | Jan -> 1
  | Feb -> 2
  | Mar -> 3
  | Apr -> 4
  | May -> 5
  | Jun -> 6
  | Jul -> 7
  | Aug -> 8
  | Sep -> 9
  | Oct -> 10
  | Nov -> 11
  | Dec -> 12

(** [addzeros count term] is a string 
    concatenating "0" with [term] 
    while length of [term] < [count]
*)
let rec addzeros count term = 
  if String.length term < count then addzeros count ("0" ^ term) 
  else term

let time_to_string (tm:t):string =
  match tm with
  | {year = yr; month = mn; day_m = dm; time_d = {hour = hr; minute = min}} ->
    (mn |> month_to_int |> string_of_int |> addzeros 2) ^ "/" ^
    (string_of_int dm |> addzeros 2 ) ^ "/" ^
    (string_of_int yr |> addzeros 4) ^ "/" ^ 
    (string_of_int hr |> addzeros 2) ^  ":" ^
    (string_of_int min |> addzeros 2)

let occurs_before t1 t2 =
  (* handle years first *)              
  let years = compare t1.year t2.year in
  if years > 0 then false 
  else if years < 0 then true
  (* same year, handle months *)  
  else let months = compare (month_to_int t1.month) (month_to_int t2.month) in
    if months > 0 then false 
    else if months < 0 then true
    (* same year and month, handle day *)  
    else let day = compare t1.day_m t2.day_m in
      if day > 0 then false 
      else if day < 0 then true
      (* same year and month and day, handle hour *)  
      else let hour = compare t1.time_d.hour t2.time_d.hour in
        if hour > 0 then false 
        else if hour < 0 then true
        (* same year and month and day and hour, handle minute *)  
        else let min = compare 
                 ( t1.time_d.minute) ( t2.time_d.minute) in
          if min > 0 then false 
          else if min < 0 then true
          (* same exact time*)  
          else false

(** [compare_time t1 t2] implements the standard comparison function for times. *)
let compare_time t1 t2= 
  if t1 = t2 then 0
  else if occurs_before t1 t2 then -1 
  else 1


