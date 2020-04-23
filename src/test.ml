open OUnit2
open Calendar
open Command
open State



let cal_test = parse_file "test_calendar.json"

let calname_test 
    (name : string) (caldesc: string) (cal:Calendar.t): test =
    name >:: (fun _ ->
    (assert_equal cal_test.calname caldesc)
    )

let calender_tests = 
  [
    calname_test "test_calendar.json name" "Andrew and Sam - CS3110 MS1" cal_test;
  ]

let suite =
  "Test Suite For Adaptive Calendar"  >::: List.flatten [
    calender_tests;
  ]

let _ = run_test_tt_main suite