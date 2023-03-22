type command =
  | Hello
  | Bye
  | Home

let read_input input =
  let lower = String.lowercase_ascii input in
  match lower with
  | "home" -> Home
  | _ -> Bye
