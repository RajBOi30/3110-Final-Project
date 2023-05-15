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

let get_id_from_uname (uname : string) (lst : u) =
  let rec helper (uname : string) (users : user list) =
    match users with
    | [] -> raise Not_found
    | h :: t -> if h.username = uname then h.user_id else helper uname t
  in
  helper uname lst.users

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

let get_following user_id { users } =
  let user = List.find (fun u -> u.user_id = user_id) users in
  user.following

let follow_user username user_id { users } =
  if user_id = 0 then print_string "Please sign in to follow a user.\n"
  else if
    String.lowercase_ascii (get_uname_from_id user_id { users })
    = String.lowercase_ascii username
  then print_string "You cannot follow yourself.\n"
  else
    let lowercase_username = String.lowercase_ascii username in
    let user_exists =
      List.exists
        (fun u -> String.lowercase_ascii u.username = lowercase_username)
        users
    in
    if not user_exists then print_string "The user does not exist.\n"
    else
      let is_already_following =
        let current_following =
          List.map String.lowercase_ascii (get_following user_id { users })
        in
        List.mem lowercase_username current_following
      in
      if is_already_following then
        print_string ("You are already following user: " ^ username ^ "\n")
      else
        let updated_users =
          List.map
            (fun u ->
              if u.user_id = user_id then
                let updated_following =
                  if
                    List.mem lowercase_username
                      (List.map String.lowercase_ascii u.following)
                  then u.following
                  else lowercase_username :: u.following
                in
                { u with following = updated_following }
              else u)
            users
        in
        let updated_user_list = { users = updated_users } in
        save_to_users updated_user_list;
        print_string ("You are now following user: " ^ username ^ "\n")

let view_following user_id { users } =
  if user_id = 0 then
    print_string "Please sign in to view the users you follow.\n"
  else
    let following = get_following user_id { users } in
    if List.length following = 0 then
      print_string "You are not following any users.\n"
    else begin
      print_string "Users you follow:\n";
      List.iter (fun username -> print_endline username) following
    end

let rec create_account { users } =
  print_string "\nPlease enter a username:\n";
  let username = read_line () in

  let lowercase_username = String.lowercase_ascii username in
  let user_exists =
    List.exists
      (fun u -> String.lowercase_ascii u.username = lowercase_username)
      users
  in
  if user_exists then
    print_string
      "Username already exists. Please choose a different username.\n"
  else (
    print_string "\nPlease enter a password:\n";
    let password = read_line () in

    let max_user_id = List.fold_left (fun acc u -> max acc u.user_id) 0 users in
    let new_user =
      {
        user_id = max_user_id + 1;
        password;
        username;
        listings = [];
        saved = [];
        following = [];
      }
    in
    let updated_users = new_user :: users in
    let updated_user_list = { users = updated_users } in
    save_to_users updated_user_list;
    print_string
      ("User " ^ username
     ^ " created successfully! Sign in to make your first listing, or find a \
        seller to follow! \n"))

let view_users { users } =
  print_endline "\nHere are the current users of Goofy Marketplace:";
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  List.iter
    (fun user ->
      let username = user.username in
      let user_id = user.user_id in
      let following =
        match user.following with
        | [] -> "None"
        | following_users -> String.concat ", " following_users
      in
      print_endline ("Username: " ^ username);
      print_endline ("User ID: " ^ string_of_int user_id);
      print_endline ("Following: " ^ following);
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    users

let update_user_listings user_id listing_id { users } =
  let updated_users =
    List.map
      (fun u ->
        if u.user_id = user_id then
          let updated_listings = listing_id :: u.listings in
          { u with listings = updated_listings }
        else u)
      users
  in
  let updated_user_list = { users = updated_users } in
  save_to_users updated_user_list

let suggested_user user_id { users } =
  if user_id = 0 then print_string "Please sign in to get a user suggestion.\n"
  else
    match users with
    | [] -> print_string "There are no users."
    | users ->
        let seed = int_of_float (Unix.time ()) in
        Random.init seed;

        (* Filter out the current user *)
        let filtered_users =
          List.filter (fun user -> user.user_id <> user_id) users
        in

        if filtered_users = [] then print_string "No other users available.\n"
        else
          let random_user =
            List.nth filtered_users (Random.int (List.length filtered_users))
          in
          print_string
            ("Here is a suggested user: " ^ random_user.username ^ "\n")
