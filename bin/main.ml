open Market
open Command
open Listing
open Users

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

let user = { id = 0; username = "NONE" }

(** [homepage ()] prints out every listing's details such as title, description,
    price, username, and date. *)
let homepage () =
  print_string (print_feed "\n\nHere are the latest listings:\n" feed)

let signin () =
  print_string "\n\nPlease enter your user id.\n\n\n";
  if user.id = 0 then
    let entered_id = read_line () in
    let num = int_of_string entered_id in
    if List.mem num ids then (
      print_string "\n\nPlease enter your password.\n\n\n";

      let password = read_line () in

      if password = get_pass_from_id num user_list then (
        user.id <- num;
        user.username <- get_uname_from_id num user_list;
        print_string ("\n\nWelcome User " ^ user.username))
      else
        print_string
          "That username password combination does not match, returning to \
           home screen.")
    else print_string "User ID not found, returning to home screen."
  else print_string "User is already authenticated"

(** [exit ()] quits the program. *)
let exit () =
  print_string "Thanks for stopping by!\n";
  exit 0

(** [welcome_page ()] prompts the user for an input and matches it with a
    command. *)
let rec welcome_page () =
  print_string
    "\n\n\
     Please enter a command (such as 'home') to explore the marketplace.\n\n";
  try
    match parse (read_line ()) with
    | Home ->
        homepage ();
        welcome_page ()
    | Quit -> exit ()
    | SignIn ->
        signin ();
        welcome_page ()
  with _ ->
    print_string "This command is invalid, or has not yet been implemented";
    welcome_page ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to Goofy Marketplace!";
  welcome_page ()

(* Execute the game engine. *)
let () = main ()
