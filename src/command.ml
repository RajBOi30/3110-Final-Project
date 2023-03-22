type object_phrase = string list

type command =
  | Home
  | Quit

exception Empty
exception Malformed

(**[input_to_list input] returns a string list of words without spaces in
   [input].*)
let input_to_list input =
  let lst = String.split_on_char ' ' input in
  List.filter (fun x -> x <> "") lst

(**[parse input] returns a ___ [input].*)
let parse input =
  let lower = String.lowercase_ascii input in
  match input_to_list lower with
  | [] -> raise Empty
  | "home" :: t -> Home
  | _ -> raise Malformed
