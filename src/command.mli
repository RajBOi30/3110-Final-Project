type command =
  | Home
  | Quit
  | SignIn

val input_to_list : string -> string list
val parse : string -> command
