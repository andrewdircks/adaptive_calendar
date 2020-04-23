open Yojson.Basic.Util
open Time

type event =  {
  starts : Time.t;
  ends : Time.t;
  name : string;
  description : string;
}

module Event_Time_Pairs = 
struct
  type t = (string * Time.t)
  let compare e1 e2 = Time.compare_time (snd e1) (snd e2)
end

module EventMap = Map.Make(Event_Time_Pairs)

(* RI : no two events have the same start time and end date. *)
type t = {
  calname: string;
  events: event list;
}

exception EventDNE

(** [event_of_json ejson] is the event that [ejson] represents. 
    Requires: [ejson] is a valid JSON calendar-event representation. *)
let event_of_json (ejson : Yojson.Basic.t) : event = 
  {
    starts = ejson |> member "starts" |> to_string |> Time.from_string;
    ends = ejson |> member "ends" |> to_string |> Time.from_string;
    name = ejson |> member "name" |> to_string;
    description = ejson |> member "description" |> to_string;
  }

let from_json json = 
  {
    events = json |> member "events" |> to_list |> List.map event_of_json;
    calname = json |> member "calname" |> to_string;
  }


let parse_file fname = 
  Yojson.Basic.from_file fname |> from_json


let to_json = failwith "unimplemented"

let get_events = failwith "unimplemented"

let todays_events = failwith "unimplemented"

(* (** [find_event name time c] is the event in [c] with name [name] and start time
    [time].
    Raises: [EventDNE] if no such event exists. *)
   let rec find_event (name : string) (time : Time.t) =  *)


let add_event = failwith "unimplemented"

let delete_event = failwith "unimplemented"

let replace_event = failwith "unimplemented"

let event_from_name_date = failwith "unimplemented"

let change_name n c = 
  {
    starts = c.starts;
    ends = c.ends;
    name = n;
    description = c.description;
  }

let change_description d c = 
  {
    starts = c.starts;
    ends = c.ends;
    name = c.name;
    description = d;
  }

let change_start_time st c = 
  {
    starts = st;
    ends = c.ends;
    name = c.name;
    description = c.description;
  }

let change_end_time et c = 
  {
    starts = c.starts;
    ends = et;
    name = c.name;
    description = c.description;
  }