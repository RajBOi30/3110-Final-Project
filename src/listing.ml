open Yojson.Basic.Util

type listing = {
  listing_id : int;
  user_id : int;
  username : string;
  title : string;
  description : string;
  price : string;
  date : string;
}

type user = {
  user_id : int;
  password : string;
  username : string;
  listings : int list;
}

type f = { feed : listing list }
type u = { users : user list }

(* Helper function for feed_from_json *)
let listing_from_json json =
  {
    listing_id = json |> member "listing id" |> to_int;
    user_id = json |> member "user id" |> to_int;
    username = json |> member "username" |> to_string;
    title = json |> member "title" |> to_string;
    description = json |> member "description" |> to_string;
    price = json |> member "price" |> to_string;
    date = json |> member "date" |> to_string;
  }

let user_from_json json =
  {
    user_id = json |> member "user id" |> to_int;
    password = json |> member "password" |> to_string;
    username = json |> member "username" |> to_string;
    listings = json |> member "listings" |> to_list |> List.map to_int;
  }

let feed_from_json json : f =
  { feed = json |> member "listings" |> to_list |> List.map listing_from_json }

let users_from_json json : u =
  { users = json |> member "users" |> to_list |> List.map user_from_json }

(**[get_listing_id x] returns the listing id of listing [x].*)
let get_listing_id x = x.listing_id

(**[get_user_id x] returns the user id of listing [x].*)
let get_user_id (x : listing) = x.user_id

(**[get_username x] returns the user id of listing [x].*)
let get_username (x : listing) = x.username

(**[get_title] returns the title of listing [x].*)
let get_title x = x.title

(**[get_desc] returns the description of listing [x].*)
let get_desc x = x.description

(**[get_price] returns the price of listing [x].*)
let get_price x = x.price

(**[get_date] returns the date of listing [x].*)
let get_date x = x.date

let single_listing listing =
  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" ^ get_title listing ^ "\n"
  ^ "Item Description: " ^ get_desc listing ^ "\n" ^ "Price: $"
  ^ get_price listing ^ "\n" ^ "Posted by: " ^ get_username listing ^ " on "
  ^ get_date listing ^ "\n"

let rec print_feed acc (lst : f) =
  match lst.feed with
  | [] -> acc
  | [ h ] -> acc ^ single_listing h
  | h :: t -> print_feed (acc ^ single_listing h) { feed = t }

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
