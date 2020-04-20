open Unix


(**To Pre-Compute Local-GMT Difference:
First we get GMT time
*)
let t_seconds_GMT = Unix.time()
(** Pass in GMT date for mktime!
* mktime makes GMT float from local date
* if input is instead GMT date:
x = GMT_date
TimeDif = curLocal - curGMT
mktime(x) = x + (curGMT - curLocal)
= x - timeDif
= GMT_seconds - timeDif
But we have the GMT_seconds!
So timeDif = GMT_seconds - makeTime

* This enables time_dif calculation!
*)

let gmt_date = Unix.gmtime t_seconds_GMT
let makeTime = fst (Unix.mktime gmt_date)
let time_dif =  (t_seconds_GMT -. makeTime)


type day = int
type month = int
type year = int

type sec = int
type min = int
type hr = int

type weekday = int

type dt = (month * day * year)
type tm = (hr * min * sec)
type t = (dt * tm * weekday)

let toLocal (gmt_time:t): t  =
  match gmt_time with
  | ((m,d,y),(hr, min, sec), _) ->
    (* tm_wday and tm_yday fields of tm are ignored *)
    let mytm = {tm_sec = sec; tm_min = min; tm_hour = hr; 
    tm_mon = m; tm_year = y-1900; tm_mday = d; 
    tm_isdst = false; tm_wday = 1;tm_yday = 1 } in
    let local_seconds = (fst (Unix.mktime mytm))
    +. 2.0 *. time_dif in
    let loc = Unix.gmtime local_seconds in
    ((loc.tm_mon, loc.tm_mday, loc.tm_year + 1900),
    (loc.tm_hour, loc.tm_min, loc.tm_sec),
    loc.tm_wday)

let toGMT (local_time: t): t = 
  match local_time with
  | ((m,d,y),(hr, min, sec), _) ->
  (* tm_wday and tm_yday fields of tm are ignored *)
  let mytm = {tm_sec = sec; tm_min = min; tm_hour = hr; 
  tm_mon = m; tm_year = y-1900; tm_mday = d; 
  tm_isdst = false; tm_wday = 1;tm_yday = 1 } in
  let gmt_seconds = (fst (Unix.mktime mytm)) in 
  let gmt = (Unix.gmtime gmt_seconds) in
  ((gmt.tm_mon, gmt.tm_mday, gmt.tm_year + 1900),
    (gmt.tm_hour, gmt.tm_min, gmt.tm_sec),
    gmt.tm_wday)
