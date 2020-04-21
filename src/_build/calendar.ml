open Time

type event = {
  starts : Time.t;
  ends : Time.t;
  name : string;
  description : string;
}

type t = event list

exception EventDNE

let from_json = failwith "unimplemented"

let to_json = failwith "unimplemented"

let get_events = failwith "unimplemented"

let todays_events = failwith "unimplemented"

let add_event = failwith "unimplemented"

let delete_event = failwith "unimplemented"

let replace_event = failwith "unimplemented"

let event_from_name_date = failwith "unimplemented"
