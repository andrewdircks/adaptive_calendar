open Command

exception InvalidMode

type t = Main | View

let is_valid_action cmd t =
  ()