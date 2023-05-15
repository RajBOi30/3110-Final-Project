(** Code for parsing user commands. *)

(** The type command represents a user inputted command. Invariant: any command
    that carries another type must not be empty.*)
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
  | SuggestedPost
  | SuggestedUser

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val input_to_list : string -> string list
(**[input_to_list input] returns a string list of lowercase words without spaces
   in [input].*)

val parse : string -> command
(**[parse input] returns a command given an [input]. Inputs that are more than
   one word are segmented by spaces and parsed normally. *)
