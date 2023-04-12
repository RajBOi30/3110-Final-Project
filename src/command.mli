type command =
  | Home
  | Quit
  | SignIn

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val input_to_list : string -> string list
val parse : string -> command
