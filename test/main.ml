open OUnit2
open Market
open Listing
open Users
open Command
open Yojson.Basic.Util


(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let data_dir_prefix = "data" ^ Filename.dir_sep
let user_data_dir_prefix = "data/" ^ "userData" ^ Filename.dir_sep
let user_list = Yojson.Basic.from_file (user_data_dir_prefix ^ "test_users.json")
let listings = Yojson.Basic.from_file (data_dir_prefix ^ "listings.json")

(* Helper Functions for Testing *)
let command_test (name : string) (str : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let is_valid_price_test (name : string) (str : string) (expected_output : bool)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (is_valid_price str) ~printer:string_of_bool

let is_valid_date_test (name : string) (str : string) (expected_output : bool) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (is_valid_date str) ~printer:string_of_bool


let test_file = Yojson.Basic.from_file "data/listings.json"

let listing_from_json_test (name : string) (json : Yojson.Basic.t)
    (expected_output : listing) : test =
  name >:: fun _ -> assert_equal expected_output (listing_from_json json)

(* Test Cases *)
let command_tests =
  [
    ( "Blank input" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "") );
    ( "All spaces input" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "          ") );
    ( "Command does not exist" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "leave") );
    command_test "Quit command extra space" "quit " Quit;
    command_test "Quit command mixed case" "QUit" Quit;
    command_test "Home command uppercase" "  HOME" Home;
    command_test "Home command mixed case" "hOme" Home;
    command_test "Sign In one word case" "SignIn" SignIn;
    command_test "Sign In two word case" "Sign In" SignIn;
    command_test "Sign In lower case test" "signin" SignIn;
  ]

let is_valid_price_tests =
  [
    is_valid_price_test
      "This string represents a valid currency format since it has two digits \
       after the decimal point."
      "2.22" true;
    is_valid_price_test
      "This string is invalid because it has three digits after the decimal \
       point."
      "10.123" false;
    is_valid_price_test
      "This string is valid since it has one digit after the decimal point."
      "0.5" true;
    is_valid_price_test
      "This string is invalid because it doesn't have a decimal point." "3" true;
    is_valid_price_test
      "This string is invalid since it contains non-numeric characters." "abc"
      false;
    is_valid_price_test
      "This string is invalid because it has three digits after the decimal \
       point."
      "1.234" false;
    is_valid_price_test
      "An empty string is considered invalid because it doesn't follow the \
       currency format."
      "" false;
    is_valid_price_test
      "This string is valid as it has two zeros after the decimal point." "0.00"
      true;
    is_valid_price_test
      "This string is invalid because it doesn't have a decimal point or \
       digits after it."
      "100" false;
    is_valid_price_test
      "This string is invalid because it doesn't have digits after the decimal \
       point."
      "123." false;
    is_valid_price_test
      "This string is invalid because it doesn't have digits after the decimal \
       point."
      ".25" false;
  ]

let is_valid_date_tests =
  [
    is_valid_date_test
      "This date is valid as it follows the format \"MM/DD/YY\" and satisfies \
       the range conditions for the month, day, and year."
      "01/15/22" true;
    is_valid_date_test
      "This date is valid because it represents a leap year, and February 29th \
       is a valid date in a leap year."
      "02/29/20" true;
    is_valid_date_test
      "This date is valid as it satisfies the format and range conditions. \
       Note that the year 99 is treated as 1999 according to the code logic."
      "12/31/99" true;
    is_valid_date_test
      "This date is invalid because the month value 13 is greater than the \
       maximum value of 12."
      "13/01/2023" false;
    is_valid_date_test
      "This date is invalid since it does not satisfies the format and range \
       conditions."
      "01/01/2023" false;
    is_valid_date_test
      "This date is valid as it represents the year 2000 (treated as 2000), \
       which is a leap year, and February 29th is a valid date."
      "01/01/00" true;
    is_valid_date_test
      "This date is valid as it satisfies the format and range conditions."
      "12/01/23" true;
    is_valid_date_test
      "This date is invalid because the month value 00 is not within the valid \
       range (1 to 12)."
      "00/01/23" false;
    is_valid_date_test
      "This date is invalid because the day value 00 is not within the valid \
       range (1 to 31)."
      "01/00/23" false;
    is_valid_date_test
      "This date is invalid because the year value 100 is not within the valid \
       range (0 to 99)."
      "01/01/100" false;
    is_valid_date_test
      "This date is invalid because the year value 20000 is not within the \
       valid range (0 to 99)."
      "01/01/20000" false;
    is_valid_date_test
      "This date is invalid because it contains extra characters after the \
       valid date format."
      "01/01/2023/test" false;
  ]

let test_file = Yojson.Basic.from_file "data/listings_TEST.json"

let feed =
  feed_from_json
    (Yojson.Basic.from_file (data_dir_prefix ^ "listings_TEST.json"))

let listing1 = get_listing 1 feed

let listing_from_json_tests =
  [ listing_from_json_test "test" test_file listing1 ]

let users_tests = []

let suite =
  "test suite for Market"
  >::: List.flatten
         [
           command_tests;
           is_valid_price_tests;
           is_valid_date_tests;
           is_valid_date_tests;
           listing_from_json_tests;
         ]

let _ = run_test_tt_main suite
