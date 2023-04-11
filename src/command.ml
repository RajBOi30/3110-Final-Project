type object_phrase = string list

type command =
  | Home
  | Quit
  | MyListing

exception Empty
exception Malformed

(**[input_to_list input] returns a string list of lowercase words without spaces in
   [input].*)
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
  | _ -> raise Malformed
