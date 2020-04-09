open Unix

(** [get_timezone_difference] is the number of hours difference between the 
    local machine's timezone and UTC. If positive, the local machine is ahead, 
    and if negative behind. *)
let get_timezone_difference (x:int) : int= 
  let t = Unix.time() in
  let local = Unix.localtime t in
  let gm = Unix.gmtime t in 
  let difference = (gm.tm_hour - local.tm_hour) in
  if difference < -12 then -difference -24 else difference

