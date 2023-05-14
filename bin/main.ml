open Market
open Command
open Listing
open Users
open Yojson

let data_dir_prefix = "data" ^ Filename.dir_sep
let data_dir_prefix_user = "data/userData" ^ Filename.dir_sep

let feed =
  feed_from_json (Yojson.Basic.from_file (data_dir_prefix ^ "listings.json"))

let user_list =
  users_from_json (Yojson.Basic.from_file (data_dir_prefix_user ^ "users.json"))

let ids = id_list user_list

type user_rec = {
  mutable id : int;
  mutable username : string;
}

let user = { id = 0; username = "None" }

(** [homepage ()] prints out every listing's details such as title, description,
    price, username, and date. *)
let homepage () =
  print_string (print_feed "\nHere are the latest listings:\n" feed)

let signin () =
  print_string "\nPlease enter your user id.\n\n";
  if user.id = 0 then
    let entered_id = read_line () in
    let num = int_of_string entered_id in
    if List.mem num ids then (
      print_string "\nPlease enter your password.\n\n";

      let password = read_line () in

      if password = get_pass_from_id num user_list then (
        user.id <- num;
        user.username <- get_uname_from_id num user_list;
        print_string
          ("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nWelcome User "
         ^ user.username ^ "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
      else
        print_string
          "That username password combination does not match, returning to \
           home screen.\n")
    else print_string "\nUser ID not found, returning to home screen.\n"
  else print_string "\nUser is already authenticated\n"

let signout () =
  if user.id = 0 then print_string "\nThere is no user signed in.\n"
  else print_string "\nAre you sure you want to log out (Y or N)?\n";
  let decision = read_line () in
  match decision with
  | "Y" | "y" ->
      print_string ("\nSuccessfully signed out user: " ^ user.username ^ ".\n");
      user.id <- 0;
      user.username <- "None"
  | _ -> print_string "\n\nYou have not been signed out. Returning to Home.\n"

(** [exit ()] quits the program. *)
let exit () =
  print_string "Thanks for stopping by!\n";
  exit 0

let my_listings () =
  print_string (print_myfeed user.id "\nHere are your current listings:\n" feed)

(** [welcome_page ()] prompts the user for an input and matches it with a
    command. *)
let rec welcome_page () =
  print_string
    ("\n\n\
      Please enter a command (such as 'home') to explore the marketplace.\n\
      (Current User: " ^ user.username ^ ")\n\n");
  try
    match parse (read_line ()) with
    | Home ->
        homepage ();
        welcome_page ()
    | Quit -> exit ()
    | MyListing ->
        my_listings ();
        welcome_page ()
    | SignIn ->
        signin ();
        welcome_page ()
    | SignOut ->
        signout ();
        welcome_page ()
    | Like i ->
        like_post i feed;
        print_endline ("You have liked post " ^ string_of_int i ^ ".");
        welcome_page ()
  with _ ->
    print_string "This command is invalid, or has not yet been implemented";
    welcome_page ()

type state = { user : user_rec }

let get_state = { user }

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to Goofy Marketplace!";
  welcome_page ()

(* Execute the game engine. *)
let () = main ()
