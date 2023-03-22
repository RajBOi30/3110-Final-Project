open Market
open Command

let rec homepage () = print_string "\n\nHome page: \n\n"

let rec welcome_page () =
  print_string
    "\n\n\nPlease enter a command (such as 'main') to get started.\n\n";
  try
    match read_input (read_line ()) with
    | Home -> homepage ()
    | _ ->
        print_string "This command is invalid, or has not yet been i";
        welcome_page ()
  with _ ->
    print_string "This command is invalid, or has not yet been i";
    welcome_page ()

let main () =
  print_string "\n\nWelcome to Goofy Market!";
  welcome_page ()

let () = main ()
