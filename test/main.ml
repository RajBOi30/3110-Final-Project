open OUnit2
open Market
open Command

(* Helper Functions from A2's Test Suite *)

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
let users = Yojson.Basic.from_file (user_data_dir_prefix ^ "users.json")
let listings = Yojson.Basic.from_file (data_dir_prefix ^ "listings.json")

(* Helper Functions for Testing *)
let command_test (name : string) (str : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (parse str)

(* Test Cases *)
let command_tests =
  [
    ( "Blank input" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "") );
    ( "All spaces input" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "          ") );
    ( "Command does not exist" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "leave") );
    ( "Quit command with extra phrase" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "quit market") );
    command_test "Quit command extra space" "quit " Quit;
    command_test "Quit command mixed case" "QUit" Quit;
    command_test "Home command uppercase" "  HOME" Home;
    command_test "Home command mixed case" "hOme" Home;
  ]

let suite = "test suite for Market" >::: List.flatten [ command_tests ]
let _ = run_test_tt_main suite
