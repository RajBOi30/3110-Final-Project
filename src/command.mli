type command =
  | Home
  | Quit
  | MyListing
  | SignIn
  | SignOut
  | Like of int
  | Purchase of int
  | Help
  | Post
  | Reviews
  | Save of int
  | MySaved
  | SavedIDs
  | Follow of string
  | ViewFollowing
  | CreateAccount
  | ViewUsers

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val input_to_list : string -> string list
val parse : string -> command
