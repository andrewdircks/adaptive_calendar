open Unix

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
     else gm.tm_hour - local.tm_hour)

let today = 
  let local = Unix.localtime (Unix.time()) in 
  (local.tm_year + 1900, local.tm_mon, local.tm_wday, local.tm_mday)

let is_leap_year = 
  let y = (Unix.time() |> Unix.localtime).tm_year + 1900 in 
  y mod 4 = 0 && (if y mod 100 = 0 then y mod 400 = 0 else true)