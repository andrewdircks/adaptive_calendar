open Calendar
open Time
let col = ref 0

let incr c = c := (!c + 1); c

(** [month_string m] is the month that [f] represents. 
    Requires: [f] is a string in "01"..."12"*)
let month_string m = 
  match m with 
  | "01" -> "January"
  | "02" -> "February"
  | "03" -> "March"
  | "04" -> "April"
  | "05" -> "May"
  | "06" -> "June"
  | "07" -> "July"
  | "08" -> "August"
  | "09" -> "September"
  | "10" -> "October"
  | "11" -> "November"
  | "12" -> "December"
  | _ -> raise Command.InvalidDate

let int_to_background i = 
  if !i mod 3 = 0 then ANSITerminal.on_yellow 
  else if !i mod 3 = 1 then ANSITerminal.on_cyan
  else ANSITerminal.on_green

(** [day_ofm_string d] is the day of the month that [d] represents. 
    Requires: [d] is a string in "01"..."31"*)
let day_ofm_string d = 
  if int_of_string d < 10 then String.sub d 1 1 
  else d

(** [time_ofd_string d] is the time of dat that [d] represents. 
    Requires: [d] is a string in the format "hh:mm"*)
let time_ofd_string d = 
  let hr = int_of_string (String.sub d 0 2) in 

  (* pm case *)
  if hr > 12 then (string_of_int (hr - 12)) ^ ":" ^ (String.sub d 3 2) ^ " PM"

  (* am case *)
  else (string_of_int hr) ^ ":" ^ (String.sub d 3 2) ^ " AM"

let to_display_time t = 
  let t' = Time.time_to_string t in 
  let month = month_string (String.sub t' 0 2) in 
  let day = day_ofm_string (String.sub t' 3 2) in 
  let hour = time_ofd_string (String.sub t' 11 5) in
  month ^ " " ^ day ^ ", " ^ "at " ^ hour

let disp_hour t = 
  let t' = Time.time_to_string t in 
  let hour = time_ofd_string (String.sub t' 11 5) in
  hour

let head_display_time t = 
  let t' = Time.time_to_string t in
  let month = month_string (String.sub t' 0 2) in 
  let day = day_ofm_string (String.sub t' 3 2) in 
  let year = (String.sub t' 6 4) in
  month ^ " " ^ day ^ ". " ^ year

let view_event e = 
  let name = e.name in 
  let timestr ="\t"^  disp_hour (Time.toLocal e.starts) ^ " to " ^ disp_hour(Time.toLocal e.ends) ^ ":" in
  let background = int_to_background (incr col) in

  (* erase everything above the event *)
  (* ANSITerminal.erase Above; *)

  (* print event start/end times *)
  ANSITerminal.(print_string [black; Bold; background] ("\n\n" ^ timestr));
  (* print event name *)
  ANSITerminal.(print_string [black; Bold; background] ("\n\n\t" ^ name ^ "\n" ));


  (* print description *)
  ANSITerminal.(print_string [black; Reset; background] ("\t" ^ e.description ^ "\n" ))
let print_no_events () = 
  ANSITerminal.(print_string [black; Bold] ("\nNo events are scheduled for this week.\n \n"))


let deliver_header evt = 
  let locs = Time.toLocal evt.starts in
  let wk = locs |> day_of_week in 
  let time_header = (head_display_time locs) in
  ANSITerminal.(print_string [Reset] "\n");
  ANSITerminal.(print_string [black; Underlined] ("\n" ^ wk));
  ANSITerminal.(print_string [black; Bold] (" (" ^ time_header ^ ")\n") )

let rec view_multiple_events es empty_start prevday =
  match es with
  | [] -> if empty_start then print_no_events () else ()
  | h::t -> 
  let loctime = Time.toLocal h.starts in
  (if loctime.day_m <> prevday then (deliver_header h)
  else ());

  view_event h;view_multiple_events t false (loctime.day_m)


  (**view_event h; view_multiple_events t false *)

let view_week c s = 
  view_multiple_events (sort_events (Calendar.get_week s c)) true (-1)

let view_single evt = 
  view_multiple_events [evt] true (-1)