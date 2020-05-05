open Yojson.Basic.Util
open Time

type event =  {
  starts : Time.t;
  ends : Time.t;
  name : string;
  description : string;
}

(**
   Draft Code (Kept for future purposes)
   module Event_Time_Pairs = 
   struct
   type t = (string * Time.t)
   let compare e1 e2 = Time.compare_time (snd e1) (snd e2)
   end

   module EventMap = Map.Make(Event_Time_Pairs)
*)

(* RI : no two events have the same start time and end date. *)
type t = {
  calname: string;
  events: event list;
}

exception EventDNE

exception CannotAddExisting

exception MalformedTuple

let empty name = {
  calname = name;
  events = []
}

(** [event_of_json ejson] is the event that [ejson] represents. 
    Requires: [ejson] is a valid JSON calendar-event representation. *)
let event_of_json (ejson : Yojson.Basic.t) : event = 
  {
    starts = ejson |> member "starts" |> to_string |> Time.from_json_string;
    ends = ejson |> member "ends" |> to_string  |> Time.from_json_string;
    name = ejson |> member "name" |> to_string ;
    description = ejson |> member "description" |> to_string ;
  }

let from_json (json : Yojson.Basic.t) : t = 
  {
    events = json |> member "events" |> to_list |> List.map event_of_json;
    calname = json |> member "calname" |> to_string;
  }

let parse_file (fname:string) : t = 
  Yojson.Basic.from_file fname |> from_json

(** [events_to_string evtlist] is the events
    of [evtlist] formatted into a json string.
    Requires: [evtlist] is valid event list. *)
let rec events_to_string (evtlist: event list) : string = 
  if (List.length evtlist = 0) then "" else
    let curevt = List.hd evtlist in 
    "    {\n      \"starts\": \"" ^ 
    Time.time_to_string curevt.starts ^
    "\",\n      \"ends\": \"" 
    ^ Time.time_to_string curevt.ends ^
    "\",\n      \"name\": \"" ^ curevt.name ^
    "\",\n      \"description\": \"" ^ curevt.description ^
    "\"\n    }" ^ 

    (match evtlist with 
     | [] -> "\n"
     | h::[] -> "\n"
     | h::g -> ",\n" ^ events_to_string g)

(** [to_json_string cal] is the the calendar [cal]
    formatted as a json string.
    Requires: [cal] is a valid calendar. *)
let to_json_string (cal: t) : string = 
  "{\n  \"events\": [\n" ^ 
  events_to_string cal.events ^ 
  "  ],\n  \"calname\": \"" ^ cal.calname
  ^ "\"\n}"


let to_json c = 
  let newfile = open_out (c.calname ^ ".json") in 
  Printf.fprintf newfile "%s" (to_json_string c);
  close_out newfile

(* 
  Possible Future Code:
  (** [find_event name time c] is the event in [c] with name [name] and start time
    [time].
    Raises: [EventDNE] if no such event exists. *)
   let rec find_event (name : string) (time : Time.t) =   *)

(** [mem name start events] is true if there exists an event in [events] with 
    start time [start] and name [name]. False otherwise. *) 
let rec mem (name : string) (start : Time.t) (events : event list) : bool = 
  match events with 
  | [] -> false
  | h::t -> 
    if (h.name = name && h.starts = start) then true 
    else mem name start t

(** [has_name n e] is true of event [e] has name [n] and false otherwise. *)
let has_name (n : string) (e : event) : bool = e.name = n

let find_event c name = 
  let es = List.filter (has_name name) c.events in
  match List.filter (has_name name) c.events with 
  | [] -> None
  | h::t -> Some es

let add_event c e = 
  if (mem e.name e.starts c.events) then raise CannotAddExisting
  else 
    { calname = c.calname; events = (e :: c.events)}


let delete_event c e = 
  {
    calname = c.calname;
    events = List.filter (fun x -> x <> e) c.events;
  }

let change_name n e = 
  {
    starts = e.starts;
    ends = e.ends;
    name = n;
    description = e.description;
  }

let change_description d e = 
  {
    starts = e.starts;
    ends = e.ends;
    name = e.name;
    description = d;
  }

let change_start_time st e = 
  {
    starts = st |> Time.toGMT;
    ends = e.ends;
    name = e.name;
    description = e.description;
  }

let change_end_time et e = 
  {
    starts = e.starts;
    ends = et |> Time.toGMT;
    name = e.name;
    description = e.description;
  }

let first t = match t with (x, _, _) -> x
let second t = match t with (_, x, _) -> x
let third t = match t with (_, _, x) -> x

let edit_event c info = 
  try ( 
    let event = first info in
    let field = second info in 
    let value = third info in 
    let new_event = 
      if field = "name" then change_name value event 
      else if field = "description" then change_description value event 
      else if field = "start" then change_start_time (value |> Time.from_json_string) event
      else if field = "end" then change_end_time (value |> Time.from_json_string) event
      else raise EventDNE
    in 
    let deleted = delete_event c event in
    {
      calname = c.calname;
      events = new_event :: deleted.events
    }
  )
  with 
  | EventDNE -> raise EventDNE 
