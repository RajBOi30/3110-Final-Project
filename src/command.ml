type object_phrase = string list
type command = 
| Home
| Quit

exception Empty
exception Malformed

let input_to_list input =
  let lst = String.split_on_char ' ' input in
  List.filter (fun x -> x <> "") lst

let parse input =
  match input_to_list input with
  | [] -> raise Empty
  | h :: t when h = "home" && List.length t == 0 -> Printf.printf "Result: %s\n"
  | _ -> raise Malformed
