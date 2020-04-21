open Time

type event = {
  starts : Time.t;
  ends : Time.t;
  name : string;
  description : string;
}

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

let to_json = failwith "unimplemented"

let get_events = failwith "unimplemented"

let todays_events = failwith "unimplemented"

let add_event = failwith "unimplemented"

let delete_event = failwith "unimplemented"

let replace_event = failwith "unimplemented"

let event_from_name_date = failwith "unimplemented"
