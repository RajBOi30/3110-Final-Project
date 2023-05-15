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

let get_listing_test (name : string) (int : int) (f : f)
    (expected_output : listing) : test =
  name >:: fun _ -> assert_equal expected_output (get_listing int f)

let get_listing_id_test (name : string) (listing : listing)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (get_listing_id listing)

let get_user_id_test (name : string) (listing : listing) (expected_output : int)
    : test =
  name >:: fun _ -> assert_equal expected_output (get_user_id listing)

let get_username_test (name : string) (listing : listing)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_username listing)

let get_title_test (name : string) (listing : listing)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_title listing)

let get_desc_test (name : string) (listing : listing) (expected_output : string)
    : test =
  name >:: fun _ -> assert_equal expected_output (get_desc listing)

let get_price_test (name : string) (listing : listing)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_price listing)

let get_date_test (name : string) (listing : listing) (expected_output : string)
    : test =
  name >:: fun _ -> assert_equal expected_output (get_date listing)

let get_likes_test (name : string) (listing : listing) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_likes listing)

let get_reviews_test (name : string) (listing : listing)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (get_reviews listing)

let single_listing_test (name : string) (listing : listing)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:(fun x -> x) expected_output (single_listing listing)

let print_myfeed_test (name : string) (id : int) (acc : string) (feed : f)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:(fun x -> x) expected_output (print_myfeed id acc feed)

let get_uname_test (name : string) (id : int) (lst : u)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_uname_from_id id lst)

let print_feed_test (name : string) (acc : string) (feed : f)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:(fun x -> x) expected_output (print_feed acc feed)

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
let listing2 = get_listing 2 feed
let listing3 = get_listing 3 feed

let get_listing_tests =
  [
    get_listing_test "Gets the listing 1 from listing feed" 1 feed listing1;
    get_listing_test "Gets the listing 2 from listing feed" 2 feed listing2;
    get_listing_test "Gets the listing 3 from listing feed" 3 feed listing3;
    get_listing_id_test "The listing_id of listing1 is 1" listing1 1;
    get_listing_id_test "The listing_id of listing2 is 2" listing2 2;
    get_listing_id_test "The listing_id of listing3 is 3" listing3 3;
    get_user_id_test "The user_id of listing1 is 1" listing1 1;
    get_user_id_test "The user_id of listing2 is 1" listing2 1;
    get_user_id_test "The user_id of listing3 is 3" listing3 3;
    get_username_test "The username of listing1 is \"RajSinha999\"" listing1
      "RajSinha999";
    get_username_test "The username of listing2 is \"RajSinha999\"" listing2
      "RajSinha999";
    get_username_test "The username of listing3 is \"Kaylin\"" listing3 "Kaylin";
    get_title_test "The title of listing1 is \"Xbox Controller\"" listing1
      "Xbox Controller";
    get_title_test "The title of listing2 is \"Kirby Plushie\"" listing2
      "Kirby Plushie";
    get_title_test "The title of listing3 is \"Glasses\"" listing3 "Glasses";
    get_desc_test
      "The description of listing1 is \"Used Xbox 1 controller, broken left \
       joystick\""
      listing1 "Used Xbox 1 controller, broken left joystick";
    get_desc_test "The description of listing2 is \"Used Plushie\"" listing2
      "Used Plushie";
    get_desc_test "The description of listing3 is \"-8.5 eyesight glasses\""
      listing3 "-8.5 eyesight glasses";
    get_price_test "The price of listing1 is \"13.99\"" listing1 "13.99";
    get_price_test "The price of listing2 is \"4.00\"" listing2 "4.00";
    get_price_test "The price of listing3 is \"69.00\"" listing3 "69.00";
    get_date_test "The date of listing1 is \"3/21/23\"" listing1 "3/21/23";
    get_date_test "The date of listing2 is \"2/2/23\"" listing2 "2/2/23";
    get_date_test "The date of listing3 is \"3/3/23\"" listing3 "3/3/23";
    get_likes_test "The likes of listing1 is 1" listing1 1;
    get_likes_test "The likes of listing2 is 0" listing2 0;
    get_likes_test "The likes of listing3 is 1" listing3 1;
    get_reviews_test "The reviews of listing1 is []" listing1 [];
    get_reviews_test "The reviews of listing2 is []" listing2 [];
    single_listing_test
      "The single_listing prints out the listing1 on the terminal interface"
      listing1
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Xbox Controller\n\
       ID: 1\n\
       Item Description: Used Xbox 1 controller, broken left joystick\n\
       Price: $13.99\n\
       Posted by: RajSinha999 on 3/21/23\n\
       Likes: 1\n";
    single_listing_test
      "The single_listing prints out the listing2 on the terminal interface"
      listing2
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Kirby Plushie\n\
       ID: 2\n\
       Item Description: Used Plushie\n\
       Price: $4.00\n\
       Posted by: RajSinha999 on 2/2/23\n\
       Likes: 0\n";
    single_listing_test
      "The single_listing prints out the listing3 on the terminal interface"
      listing3
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glasses\n\
       ID: 3\n\
       Item Description: -8.5 eyesight glasses\n\
       Price: $69.00\n\
       Posted by: Kaylin on 3/3/23\n\
       Likes: 1\n";
    print_myfeed_test
      "The print_myfeed prints out the user 1's listing from feed on the \
       terminal interface"
      1 "\nHere are your current listings:\n" feed
      "\n\
       Here are your current listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Xbox Controller\n\
       ID: 1\n\
       Item Description: Used Xbox 1 controller, broken left joystick\n\
       Price: $13.99\n\
       Posted by: RajSinha999 on 3/21/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Kirby Plushie\n\
       ID: 2\n\
       Item Description: Used Plushie\n\
       Price: $4.00\n\
       Posted by: RajSinha999 on 2/2/23\n\
       Likes: 0\n";
    print_myfeed_test
      "The print_myfeed prints out the user 2's listing from feed on the \
       terminal interface"
      2 "\nHere are your current listings:\n" feed
      "\n\
       Here are your current listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Boba\n\
       ID: 5\n\
       Item Description: A singular Boba\n\
       Price: $11.11\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Straw\n\
       ID: 6\n\
       Item Description: A straw for your boba\n\
       Price: $22.22\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 4\n";
    print_myfeed_test
      "The print_myfeed prints out the user 3's listing from feed on the \
       terminal interface"
      3 "\nHere are your current listings:\n" feed
      "\n\
       Here are your current listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glasses\n\
       ID: 3\n\
       Item Description: -8.5 eyesight glasses\n\
       Price: $69.00\n\
       Posted by: Kaylin on 3/3/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Phone Case\n\
       ID: 4\n\
       Item Description: Blue iPhone 14 case\n\
       Price: $10.00\n\
       Posted by: Kaylin on 3/7/23\n\
       Likes: 3\n";
    print_feed_test
      "The print_feed prints out the feed on the terminal interface"
      "\nHere are the latest listings:\n" feed
      "\n\
       Here are the latest listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Xbox Controller\n\
       ID: 1\n\
       Item Description: Used Xbox 1 controller, broken left joystick\n\
       Price: $13.99\n\
       Posted by: RajSinha999 on 3/21/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Kirby Plushie\n\
       ID: 2\n\
       Item Description: Used Plushie\n\
       Price: $4.00\n\
       Posted by: RajSinha999 on 2/2/23\n\
       Likes: 0\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glasses\n\
       ID: 3\n\
       Item Description: -8.5 eyesight glasses\n\
       Price: $69.00\n\
       Posted by: Kaylin on 3/3/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Phone Case\n\
       ID: 4\n\
       Item Description: Blue iPhone 14 case\n\
       Price: $10.00\n\
       Posted by: Kaylin on 3/7/23\n\
       Likes: 3\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Boba\n\
       ID: 5\n\
       Item Description: A singular Boba\n\
       Price: $11.11\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Straw\n\
       ID: 6\n\
       Item Description: A straw for your boba\n\
       Price: $22.22\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 4\n";
  ]

let user_list =
  users_from_json
    (Yojson.Basic.from_file
       ("data/userData" ^ Filename.dir_sep ^ "test_users.json"))

let ids = id_list user_list

let users_tests =
  [
    (* Get Username Tests*)
    get_uname_test "Test that user 1 can be queryed by user id number" 1
      user_list "RajSinha999";
    get_uname_test "Test that user 4 can be queryed by user id number" 4
      user_list "peppapig";
    get_uname_test "Test that user 2 can be queryed by user id number" 2
      user_list "KevinLin21733";
    get_uname_test "Test that user 3 can be queryed by user id number" 3
      user_list "Kaylin";
  ]

let suite =
  "test suite for Market"
  >::: List.flatten
         [
           command_tests;
           is_valid_price_tests;
           is_valid_date_tests;
           is_valid_date_tests;
           get_listing_tests;
           users_tests;
         ]

let _ = run_test_tt_main suite
