open Yojson.Basic.Util
open Listing

type user = {
  user_id : int;
  password : string;
  username : string;
  listings : int list;
  saved : int list;
  following : string list;
}

type u = { users : user list }

let user_from_json json =
  {
    user_id = json |> member "user id" |> to_int;
    password = json |> member "password" |> to_string;
    username = json |> member "username" |> to_string;
    listings = json |> member "listings" |> to_list |> List.map to_int;
    saved = json |> member "saved" |> to_list |> List.map to_int;
    following = json |> member "following" |> to_list |> List.map to_string;
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

let to_yojson p : Yojson.Basic.t =
  `Assoc
    [
      ("user id", `Int p.user_id);
      ("username", `String p.username);
      ("password", `String p.password);
      ("listings", `List (List.map (fun i -> `Int i) p.listings));
      ("saved", `List (List.map (fun i -> `Int i) p.saved));
      ("following", `List (List.map (fun s -> `String s) p.following));
    ]

let file_path = "data/userData/users.json"

let save_to_users ({ users } : u) =
  let json_output user_list : Yojson.Basic.t =
    `Assoc [ ("users", `List (List.map to_yojson user_list)) ]
  in
  let yojson_user = json_output users in
  let oc = open_out file_path in
  Yojson.Basic.to_channel oc yojson_user;
  close_out oc

let save_post post_number user_id { users } =
  if user_id = 0 then print_string "Please sign in to save a post.\n"
  else
    let updated_users =
      List.map
        (fun u ->
          if u.user_id = user_id then
            let updated_saved = post_number :: u.saved in
            { u with saved = updated_saved }
          else u)
        users
    in
    let updated_user_list = { users = updated_users } in
    save_to_users updated_user_list;
    print_string ("Post " ^ string_of_int post_number ^ " has been saved.\n")

let print_saved_ids user_id { users } =
  if user_id = 0 then print_string "Please sign in to view saved post ids.\n"
  else
    let user_data = List.find (fun u -> u.user_id = user_id) users in
    let saved = user_data.saved in
    if List.length saved = 0 then print_string "You have no saved posts.\n"
    else begin
      print_string "Your saved post ids:\n";
      List.iter
        (fun post_number -> print_endline ("Post " ^ string_of_int post_number))
        saved
    end

let print_saved_posts user_id { users } =
  if user_id = 0 then print_string "Please sign in to view saved posts.\n"
  else
    let user_data = List.find (fun u -> u.user_id = user_id) users in
    let saved = user_data.saved in
    if List.length saved = 0 then print_string "You have no saved posts.\n"
    else
      let feed =
        feed_from_json
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "listings.json"))
      in
      print_string ("Your saved posts:\n" ^ print_feed_by_id saved "" feed)
