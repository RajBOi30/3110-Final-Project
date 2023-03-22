open Market
open Command
open Listing

let data_dir_prefix = "data" ^ Filename.dir_sep

let feed =
  feed_from_json (Yojson.Basic.from_file (data_dir_prefix ^ "listings.json"))

let homepage () =
  print_string (print_feed "\n\nHere are the latests posts:\n" feed)

let exit () = 
  print_string "Thanks for stopping by!\n"; exit 0
  
let rec welcome_page () =
  print_string
    "\n\n\nPlease enter a command (such as 'main') to get started.\n\n";
  try
    match parse (read_line ()) with
    | Home -> homepage (); welcome_page()
    | Quit -> exit ()
  with _ ->
    print_string "This command is invalid, or has not yet been j";
    welcome_page ()

let main () =
  print_string "\n\nWelcome to Goofy Market!";
  welcome_page ()

let () = main ()
