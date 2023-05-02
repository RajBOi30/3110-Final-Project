open Market
open Command
open Listing
open Users
open Sys

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

(* let user = { id = 0; username = "None" } *)
let user = { id = 1; username = "RajSinha999" }

type listing_rec = {
  mutable listing_id : int;
  mutable user_id : int;
  mutable username : string;
  mutable title : string;
  mutable description : string;
  mutable price : string;
  mutable date : string;
}

let listing =
  {
    listing_id = 999;
    user_id = 999;
    username = "NONE";
    title = "NONE";
    description = "NONE";
    price = "NONE";
    date = "NONE";
  }

(** [homepage ()] prints out every listing's details such as title, description,
    price, username, and date. *)
let homepage () =
  print_string (print_feed "\n\nHere are the latest listings:\n" feed)

let signin () =
  print_string "\n\n\nPlease enter your user id.\n\n";
  if user.id = 0 then
    let entered_id = read_line () in
    let num = int_of_string entered_id in
    if List.mem num ids then (
      print_string "\n\nPlease enter your password.\n\n";

      let password = read_line () in

      if password = get_pass_from_id num user_list then (
        user.id <- num;
        user.username <- get_uname_from_id num user_list;
        print_string ("\n\nWelcome User " ^ user.username ^ "\n\n\n"))
      else
        print_string
          "\n\n\
           That username password combination does not match, returning to \
           home screen.\n\n\n")
    else print_string "\n\nUser ID not found, returning to home screen.\n\n\n"
  else print_string "\n\n\nUser is already authenticated\n\n\n"

let signout () =
  if user.id = 0 then print_string "\n\n\nThere is no user signed in.\n\n\n"
  else print_string "\n\nAre you sure you want to log out (Y or N)?\n\n\n";
  let decision = read_line () in
  match decision with
  | "Y" | "y" ->
      print_string
        ("\n\nSuccessfully signed out user: " ^ user.username ^ ".\n\n\n");
      user.id <- 0;
      user.username <- "None"
  | _ ->
      print_string "\n\nYou have not been signed out. Returning to Home.\n\n\n"

(** [exit ()] quits the program. *)
let exit () =
  print_string "Thanks for stopping by!\n";
  exit 0

let my_listings () =
  print_string
    (print_myfeed user.id "\n\nHere are your current listings:\n" feed)

let new_listing () =
  if user.id = 0 then
    print_string "\n\n\nPlease sign in to make a listing.\n\n\n"
  else begin
    print_string "\n\n\nWhat is the title of your new listing?\n\n";
    listing.title <- read_line ();
    print_string "\n\n\nMake a description for the listing: \n\n";
    listing.description <- read_line ();
    print_string
      "\n\n\nWhen would you like to post this listing? (MM/DD/YY) \n\n";
    listing.date <- read_line ();
    print_string "\n\nWhat is the price of this item? \n\n";
    print_string "$";
    listing.price <- read_line ();
    print_string "\n\nHere is what your listing looks like:\n\n";
    listing.username <- user.username;
    listing.user_id <- user.id;
    print_string
      ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" ^ listing.title ^ "\n"
     ^ "Item Description: " ^ listing.description ^ "\n" ^ "Price: $"
     ^ listing.price ^ "\n" ^ "Posted by: " ^ listing.username ^ " on "
     ^ listing.date ^ "\n" ^ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n");
    print_string "Do you want to post this listing? (Y or N)\n\n";
    let decision = read_line () in
    match decision with
    | "Y" | "y" -> print_string "\n\nSuccess! Your post has been made. \n\n\n"
    (* update the json file, and the user's post count*)
    | _ -> print_string "\n\nOk, the post has been discarded.\n\n\n"
  end

(** [welcome_page ()] prompts the user for an input and matches it with a
    command. *)
let rec welcome_page () =
  print_string
    ("\n\n\
      Please enter a command (such as 'home') to explore the marketplace. \
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
    | NewListing ->
        new_listing ();
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
