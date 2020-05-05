open Calendar

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

(* [to_display_time t] is time string representing time [t] in format
   "Month Day, Year at TimeOfDay" as to be displayed by the [view_event e].*)
let to_display_time t = 
  let t' = Time.time_to_string t in 
  let month = month_string (String.sub t' 0 2) in 
  let day = day_ofm_string (String.sub t' 3 2) in 
  let hour = time_ofd_string (String.sub t' 11 5) in

  month ^ " " ^ day ^ ", " ^ "at " ^ hour

let view_event e = 
  let name = e.name in 
  let start = (Time.toLocal e.starts |> to_display_time) in 
  let ends = (Time.toLocal e.ends |> to_display_time) in 
  let desc = e.description in 

  (* erase everything above the event *)
  ANSITerminal.erase Above;

  (* print event name *)
  ANSITerminal.(print_string [black; Bold; Underlined; on_yellow] ("\n" ^ "\n" ^ name ^ "\n" ^ "\n"));

  (* print event start/end times *)
  ANSITerminal.(print_string [black; Bold; on_yellow] (start ^ "\n" ));
  ANSITerminal.(print_string [black; Bold; on_yellow] ("\n" ^" to" ^ "\n" ^ "\n"));
  ANSITerminal.(print_string [black; Bold; on_yellow] (ends ^ "\n" ^ "\n" ));

  (* print description *)
  ANSITerminal.(print_string [black; on_yellow] (desc ^ "\n"));

  print_string "\n \n \n"


let view_week c s = ()