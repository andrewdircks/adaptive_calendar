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


(** [test_increment_hour name initialtime dif expectout] 
    constructs an OUnit test named [name] asserting the equality of 
*)
(* let test_increment_hour 
    (name : string) (start_time : string) (dif:int) (expected_out_time : string) : test = 
   name >:: (fun _ ->
      assert_equal expected_out_time 
        (let s = Time.from_json_string start_time in 
         (Time.increment_hour s dif |> Time.time_to_string) 
        )) *)

let test_increment_duration
    (name : string) (start_time : Time.t) (dur : Time.time_d) (expected : Time.t) : test = 
  name >:: (fun _ ->
      assert_equal expected (Time.increment_duration start_time dur)
    )

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

let same_week_test
    (name : string) (t1 : Time.t) (t2 : Time.t) (expected : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected (same_week t1 t2)
    )

let birthdays = "birthdays.json" |> Yojson.Basic.from_file |> Calendar.from_json  

let view_parse_test
    (name : string) (cal : Calendar.t) (input : string) (expected) : test = 
  name >:: (fun _ -> 
      assert_equal expected (Command.view_parse cal input)
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


    (* test_increment_hour "testing day bef " "04/30/2020/01:00" (-10) "04/29/2020/15:00";
       test_increment_hour "testing day bef boundary" "04/30/2020/00:00" (-10) "04/29/2020/14:00";
       test_increment_hour "testing same day backwards " "02/27/2020/13:00" (-9) "02/27/2020/04:00";
       test_increment_hour "testing same day forwards " "09/01/2020/13:00" (4) "09/01/2020/17:00";
       test_increment_hour "testing next day forwards " "09/01/2020/23:00" (5) "09/02/2020/04:00";
       test_increment_hour "boundary forwards " "09/01/2020/00:00" (4) "09/01/2020/04:00"; *)

    test_increment_duration "basic minutes"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=0}}
      {hour=0; minute=15}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=15}};

    test_increment_duration "minutes accross hours"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=1; minute=45}}
      {hour=0; minute=15}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=2; minute=0}};

    test_increment_duration "minutes accross days"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=23; minute=45}}
      {hour=0; minute=15}
      { year = 2021;
        month = Sep;
        day_m = 15;
        time_d = {hour=0; minute=0}};

    test_increment_duration "basic hours"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=12; minute=45}}
      {hour=1; minute=0}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=13; minute=45}};

    test_increment_duration "basic hour and minute"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=12; minute=45}}
      {hour=1; minute=15}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=14; minute=0}};

    test_increment_duration "24 hours"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=12; minute=45}}
      {hour=24; minute=0}
      { year = 2021;
        month = Sep;
        day_m = 15;
        time_d = {hour=12; minute=45}};

    test_increment_duration "48 hours"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=12; minute=45}}
      {hour=48; minute=0}
      { year = 2021;
        month = Sep;
        day_m = 16;
        time_d = {hour=12; minute=45}};

    same_week_test "same time"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=1}}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=1}} true;

    same_week_test "t1 one minute before t2"
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=1}}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=2}} true;

    same_week_test "t1 one day after t2"
      { year = 2021;
        month = Sep;
        day_m = 15;
        time_d = {hour=0; minute=2}}
      { year = 2021;
        month = Sep;
        day_m = 14;
        time_d = {hour=0; minute=1}} false;

    same_week_test "t1 one day before t2"
      { year = 2021;
        month = Sep;
        day_m = 15;
        time_d = {hour=0; minute=2}}
      { year = 2021;
        month = Sep;
        day_m = 16;
        time_d = {hour=0; minute=1}} true;

    same_week_test "t1 one day before t2, accross months"
      { year = 2021;
        month = Apr;
        day_m = 30;
        time_d = {hour=0; minute=2}}
      { year = 2021;
        month = May;
        day_m = 1;
        time_d = {hour=0; minute=1}} true;

    same_week_test "t1 4 days day before t2, accross years"
      { year = 2020;
        month = Dec;
        day_m = 31;
        time_d = {hour=0; minute=2}}
      { year = 2021;
        month = Jan;
        day_m = 4;
        time_d = {hour=0; minute=1}} true;

    same_week_test "t1 7 days day before t2, accross years"
      { year = 2020;
        month = Dec;
        day_m = 31;
        time_d = {hour=0; minute=2}}
      { year = 2021;
        month = Jan;
        day_m = 7;
        time_d = {hour=0; minute=1}} true;

    same_week_test "t1 8 days day before t2, accross years"
      { year = 2020;
        month = Dec;
        day_m = 31;
        time_d = {hour=0; minute=2}}
      { year = 2021;
        month = Jan;
        day_m = 8;
        time_d = {hour=0; minute=1}} false;
  ]

let cmd_tests = [
  view_parse_test "just day" birthdays "9/14/2021" (Command.Week
                                                      ({
                                                        year = 2021;
                                                        month = Sep;
                                                        day_m = 14;
                                                        time_d = {hour=0; minute=1}}))
]

let suite =
  "Test Suite For Adaptive Calendar"  >::: List.flatten [
    calendar_tests;
    time_tests;
    cmd_tests;
  ]

let _ = run_test_tt_main suite