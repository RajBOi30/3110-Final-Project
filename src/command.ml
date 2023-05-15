type object_phrase = string list

type command =
  | Home
  | Quit
  | MyListing
  | SignIn
  | SignOut
  | Like of int
  | Help
  | Post
  | Save of int
  | MySaved
  | SavedIDs

exception Empty
exception Malformed

(**[input_to_list input] returns a string list of lowercase words without spaces
   in [input].*)
let input_to_list input =
  let lower = String.lowercase_ascii input in
  let lst = String.split_on_char ' ' lower in
  List.filter (fun x -> x <> "") lst

(**[parse input] returns a command given an [input].*)
let parse input =
  match input_to_list input with
  | [] -> raise Empty
  | "home" :: t -> Home
  | "quit" :: t -> Quit
  | "mylistings" :: t -> MyListing
  | "my" :: "listings" :: t -> MyListing
  | "signin" :: t -> SignIn
  | "sign" :: "in" :: t -> SignIn
  | "signout" :: t -> SignOut
  | "sign" :: "out" :: t -> SignOut
  | "help" :: t -> Help
  | "post" :: t -> Post
  | "like" :: t -> begin
      match t with
      | [ x ] -> ( try Like (int_of_string x) with _ -> raise Malformed)
      | _ -> raise Malformed
    end
  | "save" :: t -> begin
      match t with
      | [ x ] -> ( try Save (int_of_string x) with _ -> raise Malformed)
      | _ -> raise Malformed
    end
  | "my" :: "saved" :: t -> MySaved
  | "saved" :: "ids" :: t -> SavedIDs
  | "mysaved" :: t -> MySaved
  | "savedids" :: t -> SavedIDs
  | _ -> raise Malformed
