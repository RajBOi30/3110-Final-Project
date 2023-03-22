type object_phrase = string list
type command = 
| Home
| Quit

exception Empty
exception Malformed

(**[input_to_list input] returns a string list of words without spaces in [input].*)
let input_to_list input =
  let lst = String.split_on_char ' ' input in
  List.filter (fun x -> x <> "") lst

(**[parse input] returns a ___ [input].*)
let parse input =
  match input_to_list input with
  | [] -> raise Empty
  | h :: t when h = "home" && List.length t == 0 -> print_string "Title: Socks\nDescription: 2 year old socks\nPrice: $69.00\nUsername: RajBOi30\nDate: 2/31/33\n"
  | h :: t when h = "Home" && List.length t == 0 -> print_string "Title: Socks\nDescription: 2 year old socks\nPrice: $69.00\nUsername: RajBOi30\nDate: 2/31/33\n"
  | _ -> raise Malformed
