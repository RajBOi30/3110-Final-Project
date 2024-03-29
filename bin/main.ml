open Market
open Command
open Listing
open Users
open Yojson

let data_dir_prefix = "data" ^ Filename.dir_sep
let data_dir_prefix_user = "data/userData" ^ Filename.dir_sep

type user_rec = {
  mutable id : int;
  mutable username : string;
}

let user = { id = 0; username = "None" }

(** [homepage ()] prints out every listing's details such as title, description,
    price, username, and date. *)
let homepage feed =
  print_string (print_feed "\nHere are the latest listings:\n" feed)

let signin () =
  let user_list =
    users_from_json
      (Yojson.Basic.from_file (data_dir_prefix_user ^ "users.json"))
  in
  let ids = id_list user_list in
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
          "\n\
           That username password combination does not match, returning to \
           home screen.\n")
    else
      print_string
        "\n\
         User ID not found, returning to home screen. Enter VIEW USERS to \
         check your ID.\n"
  else print_string "\nUser is already authenticated\n"

let signout () =
  if user.id = 0 then print_string "\nThere is no user signed in.\n"
  else (
    print_string "\nAre you sure you want to log out (Y or N)?\n";
    let decision = read_line () in
    match decision with
    | "Y" | "y" ->
        print_string ("\nSuccessfully signed out user: " ^ user.username ^ ".\n");
        user.id <- 0;
        user.username <- "None"
    | _ -> print_string "\n\nYou have not been signed out. Returning to Home.\n")

(** [exit ()] quits the program. *)
let exit () =
  print_string "Thanks for stopping by!\n";
  exit 0

let my_listings feed =
  print_string (print_myfeed user.id "\nHere are your current listings:\n" feed)

let purchase i =
  let feed =
    feed_from_json (Yojson.Basic.from_file (data_dir_prefix ^ "listings.json"))
  in
  if user.id <> 0 then begin
    let post_id = ref i in
    if !post_id = 0 then (
      print_string
        "\n\n\n\
         Do you know the ID of the post you want to purchase? (Y or N)\n\n";
      let response = read_line () in
      begin
        match response with
        | "Y" | "y" -> ()
        | _ -> homepage feed
      end;
      print_string "\n\nPlease enter the ID:";
      let input_id = read_line () in
      post_id := int_of_string input_id);
    if String.starts_with "(*SOLD!*)" (get_desc (get_listing !post_id feed))
    then print_string "\n\nThis item has already been sold!\n\n\n"
    else (
      print_string
        ("\n\nHere is the listing you are about to purchase:\n"
        ^ single_listing (get_listing !post_id feed));
      print_string
        ("\n\nAre you sure you want to purchase this "
        ^ get_title (get_listing !post_id feed)
        ^ "? (Y or N)\n\n");
      let decision = read_line () in
      match decision with
      | "Y" | "y" -> archive_listing (get_listing !post_id feed)
      | _ -> print_string "\n\nThe purchase was not made. Returning to Home.\n")
  end
  else print_string "\n\n\nPlease sign in to make a purchase\n\n\n"

let print_bold_text text =
  print_string "\027[1m";
  print_string text;
  print_string "\027[0m";
  print_newline ()

let help () =
  print_bold_text "\n\n\nHere is a list of supported commands:";
  print_string
    "\n\
     HOME: Displays the homepage of the app with the most recent listings\n\
     QUIT: Exits the app\n\
     CREATE ACCOUNT: Create a new account for the marketplace \n\
     SIGN IN: Sign in to your account with your username and password\n\
     SIGN OUT: Sign out of your account *must be signed in*\n\
     POST: Post a listing *must be signed in*\n\
     MY LISTINGS: Displays all of your listings *must be signed in*\n\
     REVIEW: View and post reviews for any listing \n\
     PURCHASE [id]: Purchase an item with ID [id] that is currently on the \
     market *must be signed in*\n\
     LIKE [id]: Like a post with ID [id] *must be signed in*\n\
     SAVE [id]: Save an item with ID [id] that is currently on the market \
     *must be signed in* \n\
     MY SAVED: View a list of your saved items *must be signed in* \n\
     SAVED IDs: View a list of the IDs of your saved items, for a quick look \
     *must be signed in*\n\
     FOLLOW [username]: Follow the user with username [username] *must be \
     signed in*\n\
     VIEW FOLLOWING: View the current users you are following *must be signed \
     in* \n\
     VIEW USERS: View a list of the current users on the platform.\n\
     SUGGESTED POST: Randomly generates a suggested post for you to view. \n\
     SUGGESTED USER: Randomly generates a suggested user you may want to \
     follow."

let review () =
  let feed =
    feed_from_json (Yojson.Basic.from_file (data_dir_prefix ^ "listings.json"))
  in
  if user.id <> 3000 then begin
    print_string
      "\n\n\nDo you know the ID of the post you want to review? (Y or N)\n\n";
    let response = read_line () in
    begin
      match response with
      | "Y" | "y" -> ()
      | _ -> homepage feed
    end;
    print_string "\n\nPlease enter the ID of the post you want to review:\n";
    let input_id = read_line () in
    let post_id = int_of_string input_id in
    print_string
      ("\n\nHere are the current reviews for "
      ^ get_title (get_listing post_id feed)
      ^ ":\n\n");
    print_string (print_reviews (get_listing post_id feed));
    print_string "\n\nDo you want to add a review? (Y or N)\n\n";
    let decision = read_line () in
    match decision with
    | "Y" | "y" -> (
        print_string "\n\nType your review here:\n\n";
        let rev = read_line () in
        print_string
          ("\nAre sure you want to add this review to "
          ^ get_title (get_listing post_id feed)
          ^ "? (Y or N)\n\n" ^ rev ^ "\n\n");
        let answer = read_line () in
        match answer with
        | "Y" | "y" ->
            add_review (get_listing post_id feed) rev;
            print_string "\n\nYour review was added!\n\n\n"
        | _ -> print_string "\n\nOk, disregarding the review\n\n\n")
    | _ -> print_string "\n\nOk, returning to home.\n\n\n"
  end
  else print_string "\n\n\nPlease sign in to make a review\n\n\n"

(** [welcome_page ()] prompts the user for an input and matches it with a
    command. *)
let rec welcome_page () =
  let feed =
    feed_from_json (Yojson.Basic.from_file (data_dir_prefix ^ "listings.json"))
  in
  let user_list =
    users_from_json
      (Yojson.Basic.from_file (data_dir_prefix_user ^ "users.json"))
  in
  print_string
    ("\n\n\
      Please enter a command ('help' provides a list of commands) to explore \
      the marketplace. (Current User: " ^ user.username ^ ")\n\n");
  try
    match parse (read_line ()) with
    | Home ->
        homepage feed;
        welcome_page ()
    | Quit -> exit ()
    | MyListing ->
        my_listings feed;
        welcome_page ()
    | SignIn ->
        signin ();
        welcome_page ()
    | SignOut ->
        signout ();
        welcome_page ()
    | Like i ->
        like_post i user.id feed;
        welcome_page ()
    | Help ->
        help ();
        welcome_page ()
    | Purchase i ->
        purchase i;
        welcome_page ()
    | Post ->
        post user.id user.username feed;
        update_user_listings !max_id user.id user_list;
        welcome_page ()
    | Reviews ->
        review ();
        welcome_page ()
    | Save i ->
        save_post i user.id user_list;
        welcome_page ()
    | SavedIDs ->
        print_saved_ids user.id user_list;
        welcome_page ()
    | MySaved ->
        print_saved_posts user.id user_list;
        welcome_page ()
    | Follow x ->
        follow_user x user.id user_list;
        welcome_page ()
    | ViewFollowing ->
        view_following user.id user_list;
        welcome_page ()
    | CreateAccount ->
        create_account user_list;
        welcome_page ()
    | ViewUsers ->
        view_users user_list;
        welcome_page ()
    | SuggestedPost ->
        suggested_post feed;
        welcome_page ()
    | SuggestedUser ->
        suggested_user user.id user_list;
        welcome_page ()
  with _ ->
    print_string
      "This command is invalid, please enter HELP for a list of commands.";
    welcome_page ()

type state = { user : user_rec }

let get_state = { user }

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to Goofy Marketplace!";
  welcome_page ()

(* Execute the game engine. *)
let () = main ()
