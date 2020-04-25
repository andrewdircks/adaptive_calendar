open OUnit2
open Calendar
open Command
open State


(** Possible Sample Test to Experiment With 
let cal_test = parse_file "test_calendar.json"*)

(** [calname_test name caldesc cal] constructs an OUnit
    test named [name] that asserts the equality of [cal.calname]
    with [calname]. *)
let calname_test 
    (name : string) (calname: string) (cal:Calendar.t): test =
    name >:: (fun _ ->
    (assert_equal cal.calname calname)
    )

(** [calender_tests] is all the tests to be completed on the calendar record*)
let calender_tests = 
  [
    (**calname_test "test_calendar.json name" "test" cal_test;*)
  ]

let suite =
  "Test Suite For Adaptive Calendar"  >::: List.flatten [
    calender_tests;
  ]

let _ = run_test_tt_main suite