(** TEST PLAN *)

(** To ensure the validity of our system, we employed a comprehensive testing
    approach, combining both OUnit tests and manual testing. We extensively
    tested the Users, Listing, and Command modules using OUnit and employed both
    glass box testing and black box testing. For functions such as the date and
    price validation, we used glass box testing to evaluate every possible
    branch of the code. For users testing, we combined both methods of testing.
    Tests for command.ml were mostly black-box tests, where we used edge cases
    to test if various variations of the commands would work as expected. For
    functions that directly wrote to JSON, we carefully examined the resulting
    data files to confirm that the functions wrote to the expected fields.

    For manual testing, we interacted with the command line UI, implemented in
    bin/main.ml, to verify correct terminal output and proper program behavior,
    ensuring no error throws or unexpected outcomes. Since the command line
    interface is the primary user interaction point and many functions primarily
    return (), it was more effective to manually test than to create OUnit tests
    in this case.

    Our testing approach combines black box, glass box, and thorough manual
    testing to ensure the correctness of our program. The correctness of the
    program is further supported by the fact that all created posts are
    correctly displayed on the main feed with the associated data when observed
    by any user. *)

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

let user_list =
  users_from_json
    (Yojson.Basic.from_file
       ("data/userData" ^ Filename.dir_sep ^ "test_users.json"))

let test_file = Yojson.Basic.from_file "data/listings_TEST.json"
let feed = feed_from_json test_file
let feed2 = feed_from_json (Yojson.Basic.from_file "data/listings_TEST1.json")
let listing1 = get_listing 1 feed
let listing2 = get_listing 2 feed
let listing3 = get_listing 3 feed
let listing4 = get_listing 1 feed2
let listing5 = get_listing 2 feed2
let ids = id_list user_list

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

let print_feed_by_id_test (name : string) (id_list : int list) (acc : string)
    (feed : f) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal
    ~printer:(fun x -> x)
    expected_output
    (print_feed_by_id id_list acc feed)

let get_uname_test (name : string) (id : int) (lst : u)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_uname_from_id id lst)

let get_pass_from_id_test (name : string) (id : int) (lst : u)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_pass_from_id id lst)

let get_id_from_uname_test (name : string) (uname : string) (lst : u)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (get_id_from_uname uname lst)

let print_feed_test (name : string) (acc : string) (feed : f)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:(fun x -> x) expected_output (print_feed acc feed)

let id_list_test (name : string) (input : u) (expected_output : int list) : test
    =
  name >:: fun _ -> assert_equal (id_list user_list) ids

let user_following_test (name : string) (user_id : int) (lst : u)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (get_following user_id lst)

let print_reviews_test (name : string) (listings : listing)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:(fun x -> x) expected_output (print_reviews listings)

let feed_from_json_test (name : string) (json : Yojson.Basic.t)
    (expected_output : f) : test =
  name >:: fun _ -> assert_equal expected_output (feed_from_json json)

let user_from_json_test (name : string) (json : Yojson.Basic.t)
    (expected_output : user) : test =
  name >:: fun _ -> assert_equal expected_output (user_from_json json)

let to_yojson_test (name : string) (user : user)
    (expected_output : Yojson.Basic.t) : test =
  name >:: fun _ -> assert_equal expected_output (to_yojson user)

let follow_user_test (name : string) (username : string) (user_id : int)
    (users : u) (expected_output : unit) : test =
  name >:: fun _ ->
  assert_equal expected_output (follow_user username user_id users)

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
    command_test "Like command with low number post" "Like 1 " (Like 1);
    command_test "Like command with higher post number" "Like 20 " (Like 20);
    command_test "Purchase with a 0 input" "Purchase 0 " (Purchase 0);
    command_test "Purchase with a high input (Purchase 10)" "Purchase 10 "
      (Purchase 10);
    command_test
      "Test the help command with disrupted characters (different cases) "
      "hElP" Help;
    command_test "Test the standard help command" "Help" Help;
    command_test "Standard Post command" "Post" Post;
    command_test "Standard reviews command" "Reviews" Reviews;
    command_test "Reviews command but with no 's'" "Review" Reviews;
    command_test "Save a single post" "Save 1 " (Save 1);
    command_test "View saved IDS (standard type)" "Saved IDs" SavedIDs;
    command_test "Vieww Saved IDs with lower case" "saved ids" SavedIDs;
    command_test "View Follwing with no spaces" "viewfollowing" ViewFollowing;
    command_test "View Follwing with spaces" "view following" ViewFollowing;
    command_test "Create account with standard case" "Create account"
      CreateAccount;
    command_test "Create account with random cases" "create AcCount"
      CreateAccount;
    command_test "View users standard command call" "view users" ViewUsers;
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

let get_listing_tests =
  [
    get_listing_test "Gets the listing 1 from listing feed" 1 feed listing1;
    get_listing_test "Gets the listing 2 from listing feed" 2 feed listing2;
    get_listing_test "Gets the listing 3 from listing feed" 3 feed listing3;
    get_listing_test "Gets the listing 1 from listing feed2" 1 feed2 listing4;
    get_listing_test "Gets the listing 2 from listing feed2" 2 feed2 listing5;
    get_listing_id_test "The listing_id of listing1 is 1" listing1 1;
    get_listing_id_test "The listing_id of listing2 is 2" listing2 2;
    get_listing_id_test "The listing_id of listing3 is 3" listing3 3;
    get_listing_id_test "The listing_id of listing4 is 1" listing4 1;
    get_listing_id_test "The listing_id of listing5 is 2" listing5 2;
    get_user_id_test "The user_id of listing1 is 1" listing1 1;
    get_user_id_test "The user_id of listing2 is 1" listing2 1;
    get_user_id_test "The user_id of listing3 is 3" listing3 3;
    get_username_test "The username of listing1 is \"RajSinha999\"" listing1
      "RajSinha999";
    get_username_test "The username of listing2 is \"RajSinha999\"" listing2
      "RajSinha999";
    get_username_test "The username of listing3 is \"Kaylin\"" listing3 "Kaylin";
    get_username_test "The username of listing1 is \"KevinLin21733\"" listing4
      "KevinLin21733";
    get_title_test "The title of listing1 is \"Xbox Controller\"" listing1
      "Xbox Controller";
    get_title_test "The title of listing2 is \"Kirby Plushie\"" listing2
      "Kirby Plushie";
    get_title_test "The title of listing3 is \"Glasses\"" listing3 "Glasses";
    get_title_test "The title of listing4 is \"Straw\"" listing4 "Straw";
    get_desc_test
      "The description of listing1 is \"Used Xbox 1 controller, broken left \
       joystick\""
      listing1 "Used Xbox 1 controller, broken left joystick";
    get_desc_test "The description of listing2 is \"Used Plushie\"" listing2
      "Used Plushie";
    get_desc_test "The description of listing3 is \"-8.5 eyesight glasses\""
      listing3 "-8.5 eyesight glasses";
    get_desc_test "The description of listing4 is \"A straw for your boba\""
      listing4 "A straw for your boba";
    get_price_test "The price of listing1 is \"13.99\"" listing1 "13.99";
    get_price_test "The price of listing2 is \"4.00\"" listing2 "4.00";
    get_price_test "The price of listing3 is \"69.00\"" listing3 "69.00";
    get_price_test "The price of listing4 is \"22.22\"" listing4 "22.22";
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
    single_listing_test
      "The single_listing prints out the listing4 on the terminal interface"
      listing4
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Straw\n\
       ID: 1\n\
       Item Description: A straw for your boba\n\
       Price: $22.22\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 4\n";
    single_listing_test
      "The single_listing prints out the listing4 on the terminal interface"
      listing5
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glass Mug\n\
       ID: 2\n\
       Item Description: Brand new Friends glass mug!\n\
       Price: $7.80\n\
       Posted by: Kaylin on 05/14/23\n\
       Likes: 3\n";
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
      "The print_myfeed prints out the user 3's listing from feed2 on the \
       terminal interface"
      3 "\nHere are your current listings:\n" feed2
      "\n\
       Here are your current listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glass Mug\n\
       ID: 2\n\
       Item Description: Brand new Friends glass mug!\n\
       Price: $7.80\n\
       Posted by: Kaylin on 05/14/23\n\
       Likes: 3\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Coffee (*SOLD!*)\n\
       ID: 5\n\
       Item Description: (*SOLD!*)Yummy and energizing for those late nights \n\
       Price: $4.99\n\
       Posted by: Kaylin on 05/15/20\n\
       Likes: 0\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Painting (*SOLD!*)\n\
       ID: 6\n\
       Item Description: (*SOLD!*)Pretty and colorful landscape by Bob Ross\n\
       Price: $100.00\n\
       Posted by: Kaylin on 06/20/22\n\
       Likes: 0\n";
    print_myfeed_test
      "The print_myfeed prints out the user 4's listing from feed2 on the \
       terminal interface"
      4 "\nHere are your current listings:\n" feed2
      "\n\
       Here are your current listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Makeup Mirror\n\
       ID: 4\n\
       Item Description: Unbroken handheld mirror for viewing your beautiful \
       face.\n\
       Price: $9.09\n\
       Posted by: peppapig on 10/20/23\n\
       Likes: 1\n";
    print_myfeed_test
      "The print_myfeed prints out the user 4's listing from feed2 on the \
       terminal interface"
      2 "\nHere are your current listings:\n" feed2
      "\n\
       Here are your current listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Straw\n\
       ID: 1\n\
       Item Description: A straw for your boba\n\
       Price: $22.22\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 4\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: t\n\
       ID: 3\n\
       Item Description: t\n\
       Price: $11.1\n\
       Posted by: KevinLin21733 on 11/11/11\n\
       Likes: 1\n";
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
    print_feed_by_id_test
      "The feed prints out the feed of ID 1 and 3 on the terminal interface"
      [ 1; 3 ] "" feed
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Xbox Controller\n\
       ID: 1\n\
       Item Description: Used Xbox 1 controller, broken left joystick\n\
       Price: $13.99\n\
       Posted by: RajSinha999 on 3/21/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glasses\n\
       ID: 3\n\
       Item Description: -8.5 eyesight glasses\n\
       Price: $69.00\n\
       Posted by: Kaylin on 3/3/23\n\
       Likes: 1\n";
    print_feed_by_id_test
      "The feed prints out the feed of IDs 2,4 and 5 on the terminal interface"
      [ 2; 4; 5 ] "" feed
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Kirby Plushie\n\
       ID: 2\n\
       Item Description: Used Plushie\n\
       Price: $4.00\n\
       Posted by: RajSinha999 on 2/2/23\n\
       Likes: 0\n\
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
       Likes: 1\n";
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
    print_feed_test
      "The print_feed prints out the feed2 on the terminal interface"
      "\nHere are the latest listings:\n" feed2
      "\n\
       Here are the latest listings:\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Straw\n\
       ID: 1\n\
       Item Description: A straw for your boba\n\
       Price: $22.22\n\
       Posted by: KevinLin21733 on 05/14/23\n\
       Likes: 4\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Glass Mug\n\
       ID: 2\n\
       Item Description: Brand new Friends glass mug!\n\
       Price: $7.80\n\
       Posted by: Kaylin on 05/14/23\n\
       Likes: 3\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: t\n\
       ID: 3\n\
       Item Description: t\n\
       Price: $11.1\n\
       Posted by: KevinLin21733 on 11/11/11\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Makeup Mirror\n\
       ID: 4\n\
       Item Description: Unbroken handheld mirror for viewing your beautiful \
       face.\n\
       Price: $9.09\n\
       Posted by: peppapig on 10/20/23\n\
       Likes: 1\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Coffee (*SOLD!*)\n\
       ID: 5\n\
       Item Description: (*SOLD!*)Yummy and energizing for those late nights \n\
       Price: $4.99\n\
       Posted by: Kaylin on 05/15/20\n\
       Likes: 0\n\
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
       Title: Painting (*SOLD!*)\n\
       ID: 6\n\
       Item Description: (*SOLD!*)Pretty and colorful landscape by Bob Ross\n\
       Price: $100.00\n\
       Posted by: Kaylin on 06/20/22\n\
       Likes: 0\n";
    print_reviews_test "The reviews of listing1 is empty" listing1
      "There are no reviews yet for this listing. Be the first to review!";
    print_reviews_test "The reviews of listing2 is empty" listing2
      "There are no reviews yet for this listing. Be the first to review!";
  ]

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
    (*Get Password Tests*)
    get_pass_from_id_test
      "Test that user 1's password is correctly read in using their ID" 1
      user_list "watermelon";
    get_pass_from_id_test
      "Test that user 2's password is correctly read in using their ID" 2
      user_list "peaches";
    get_pass_from_id_test
      "Test that user 3's password is correctly read in using their ID" 3
      user_list "tangerine";
    get_pass_from_id_test
      "Test that user 4's password is correctly read in using their ID" 4
      user_list "oink";
    get_id_from_uname_test
      "Test that user 1's id can be found using their username" "RajSinha999"
      user_list 1;
    get_id_from_uname_test
      "Test that user 2's id can be found using their username" "KevinLin21733"
      user_list 2;
    get_id_from_uname_test
      "Test that user 3's id can be found using their username" "Kaylin"
      user_list 3;
    get_id_from_uname_test
      "Test that user 4's id can be found using their username" "peppapig"
      user_list 4;
    (*ID List Test*)
    id_list_test "Test that ID_list helper method functions correctly" user_list
      [ 4; 1; 2; 3 ];
    (*User Following Tests*)
    user_following_test
      "Check that user 1's following list is correctly read in" 1 user_list
      [ "kevinlin21733"; "kaylin" ];
    user_following_test
      "Check that user 2's following list is correctly read in" 2 user_list [];
    user_following_test
      "Check that user 2's following list is correctly read in" 3 user_list [];
    user_following_test
      "Check that user 1's following list is correctly read in" 4 user_list
      [ "rajsinha999"; "kaylin" ];
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
