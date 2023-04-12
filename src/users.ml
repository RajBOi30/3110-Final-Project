open Yojson.Basic.Util

type user = {
  user_id : int;
  password : string;
  username : string;
  listings : int list;
}

type u = { users : user list }

let user_from_json json =
  {
    user_id = json |> member "user id" |> to_int;
    password = json |> member "password" |> to_string;
    username = json |> member "username" |> to_string;
    listings = json |> member "listings" |> to_list |> List.map to_int;
  }

let users_from_json json : u =
  { users = json |> member "users" |> to_list |> List.map user_from_json }

let id_list (lst : u) : int list =
  List.fold_right (fun x acc -> x.user_id :: acc) lst.users []

let get_uname_from_id (id : int) (lst : u) =
  let rec helper (id : int) (users : user list) =
    match users with
    | [] -> raise Not_found
    | h :: t -> if h.user_id = id then h.username else helper id t
  in
  helper id lst.users

let get_pass_from_id (id : int) (lst : u) =
  let rec helper (id : int) (users : user list) =
    match users with
    | [] -> raise Not_found
    | h :: t -> if h.user_id = id then h.password else helper id t
  in
  helper id lst.users
