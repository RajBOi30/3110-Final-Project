open Market
open Command
open Listing

let data_dir_prefix = "data" ^ Filename.dir_sep

let feed =
  feed_from_json (Yojson.Basic.from_file (data_dir_prefix ^ "listings.json"))

(** [homepage ()] prints out every listing's details such as title, description,
    price, username, and date. *)
let homepage () =
  print_string (print_feed "\n\nHere are the latest listings:\n" feed)

(** [exit ()] quits the program. *)
let exit () =
  print_string "Thanks for stopping by!\n";
  exit 0

let my_listings () =
  print_string (print_myfeed "\n\nHere are your current listings:\n" feed)

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
    | MyListing ->
        my_listings ();
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
