type command =
  | Home
  | Quit
  | MyListing

val input_to_list : string -> string list
val parse : string -> command
