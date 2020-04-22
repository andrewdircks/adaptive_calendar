open OUnit2
open Calendar
open Command
open State
(** 
let get_adventure jfile =
  Yojson.Basic.from_file jfile |> from_json

Load both provided sample files
let cal_test = get_adventure "lonely_room.json"*)

let maketst 
    (name : string): test =
    name >:: (fun _ ->
    (assert_equal 1 2)
    )

let file_tests = 
  [
    maketst "running test" 
  ]

let suite =
  "Test Suite For Adaptive Calendar"  >::: List.flatten [
    file_tests;
  ]

let _ = run_test_tt_main suite
