type command =
  | Home
  | Quit

val input_to_list : string -> string list

val parse : string -> string -> command
