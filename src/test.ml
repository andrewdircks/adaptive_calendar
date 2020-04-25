open OUnit2
open Calendar
open Command
open State
open Time

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

(** [calendar_tests] is all the tests to be completed on the calendar record*)
let calendar_tests = 
  [
    (**calname_test "test_calendar.json name" "test" cal_test;*)
  ]

(** [test_time_user_conversion name user_time expected_out_time] 
    constructs an OUnit test named [name] asserting the equality of 
    time_to_string [from_input_string user_time] with [expected_out_time] *)
let test_time_user_conversion 
    (name : string) (user_time : string) (expected_out_time : string) : test = 
    name >:: (fun _ ->
    assert_equal expected_out_time 
    (Time.from_input_string user_time |> Time.time_to_string) )

(** [test_time_json_conversion name military_json_time] constructs an OUnit
    test named [name] asserting the equality of 
    [military_json_time |> from_json_string |> time_to_string] 
    with [military_json_time] *)
let test_time_json_conversion
  (name : string) (military_json_time : string) : test = 
  name >:: (fun _ -> 
  assert_equal military_json_time 
  (military_json_time |> from_json_string |> time_to_string)
  )

(** [time_tests] is all the tests to be done pertaining to Time*)
let time_tests = 
  [
    test_time_user_conversion "04/24/2020/12:30/PM" "04/24/2020/12:30/PM" 
    "04/24/2020/12:30";
    test_time_user_conversion "04/24/2020/00:30/PM" "04/24/2020/00:30/PM" 
    "04/24/2020/12:30";
    test_time_user_conversion "04/24/2020/00:30/am" "04/24/2020/00:30/am" 
    "04/24/2020/00:30";
    test_time_user_conversion "04/24/2020/12:30/AM" "04/24/2020/12:30/AM" 
    "04/24/2020/00:30";
    test_time_user_conversion "07/29/3012/07:00/AM" "07/29/3012/07:00/AM" 
    "07/29/3012/07:00";
    test_time_user_conversion "07/29/3012/07:00/PM" "07/29/3012/07:00/PM" 
    "07/29/3012/19:00";

    test_time_json_conversion "04/24/2020/12:30" "04/24/2020/12:30";
    test_time_json_conversion "04/24/2020/00:30" "04/24/2020/00:30";
    test_time_json_conversion "07/29/3012/07:00" "07/29/3012/07:00";
    test_time_json_conversion "04/24/2020/00:30" "04/24/2020/00:30";
    test_time_json_conversion "07/29/3012/19:00" "07/29/3012/19:00";
  ]

let suite =
  "Test Suite For Adaptive Calendar"  >::: List.flatten [
    calendar_tests;
    time_tests
  ]

let _ = run_test_tt_main suite