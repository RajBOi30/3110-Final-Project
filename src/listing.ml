open Yojson.Basic.Util

type listing = {
  listing_id : int;
  user_id : int;
  username : string;
  title : string;
  description : string;
  price : string;
  date : string;
  likes : int;
}

type f = { feed : listing list }

(* Helper function for feed_from_json *)
let listing_from_json json =
  let listing_id = json |> member "listing id" |> to_int in
  let user_id = json |> member "user id" |> to_int in
  let username = json |> member "username" |> to_string in
  let title = json |> member "title" |> to_string in
  let description = json |> member "description" |> to_string in
  let price = json |> member "price" |> to_string in
  let date = json |> member "date" |> to_string in
  let likes = json |> member "likes" |> to_int in
  { listing_id; user_id; username; title; description; price; date; likes }

let feed_from_json json : f =
  let listings_json = json |> member "listings" in
  let listings = listings_json |> to_list |> List.map listing_from_json in
  { feed = listings }

let to_yojson p : Yojson.Basic.t =
  `Assoc
    [
      ("listing id", `Int p.listing_id);
      ("user id", `Int p.user_id);
      ("username", `String p.username);
      ("title", `String p.title);
      ("description", `String p.description);
      ("price", `String p.price);
      ("date", `String p.date);
      ("likes", `Int p.likes);
    ]

let file_path = "data/listings.json"

let save_to_json ({ feed } : f) =
  let json_output post_list : Yojson.Basic.t =
    `Assoc [ ("listings", `List (List.map to_yojson post_list)) ]
  in
  let yojson_post = json_output feed in
  let oc = open_out file_path in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc

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

let get_likes x = x.likes

let single_listing listing =
  let title = get_title listing in
  let listing_id = get_listing_id listing in
  let description = get_desc listing in
  let price = get_price listing in
  let username = get_username listing in
  let date = get_date listing in
  let likes = get_likes listing in
  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" ^ "Title: " ^ title ^ "\n"
  ^ "ID: " ^ string_of_int listing_id ^ "\n" ^ "Item Description: "
  ^ description ^ "\n" ^ "Price: $" ^ price ^ "\n" ^ "Posted by: " ^ username
  ^ " on " ^ date ^ "\n" ^ "Likes: " ^ string_of_int likes ^ "\n"

let rec print_feed acc (lst : f) =
  match lst.feed with
  | [] -> acc
  | [ h ] ->
      let listing_str = single_listing h in
      acc ^ listing_str
  | h :: t ->
      let listing_str = single_listing h in
      let new_acc = acc ^ listing_str in
      print_feed new_acc { feed = t }

let rec print_myfeed id acc (lst : f) =
  if id = 0 then "Please sign in to view listings"
  else
    match lst.feed with
    | [] -> acc
    | [ h ] -> if get_user_id h == id then acc ^ single_listing h else acc
    | h :: t ->
        if get_user_id h == id then
          print_myfeed id (acc ^ single_listing h) { feed = t }
        else print_myfeed id acc { feed = t }

let rec print_feed_by_id (id_list : int list) acc (lst : f) =
  match lst.feed with
  | [] -> acc
  | [ h ] ->
      if List.mem h.listing_id id_list then acc ^ single_listing h else acc
  | h :: t ->
      if List.mem h.listing_id id_list then
        print_feed_by_id id_list (acc ^ single_listing h) { feed = t }
      else print_feed_by_id id_list acc { feed = t }

let like_post (i : int) (user_id : int) (feed : f) =
  if user_id <> 0 then (
    print_endline ("You have liked post " ^ string_of_int i ^ ".");
    let update p =
      if p.listing_id = i then { p with likes = p.likes + 1 } else p
    in
    let new_feed = { feed = List.map update feed.feed } in
    save_to_json new_feed)
  else print_string "\nPlease sign in to like a post."

let is_date_format (s : string) : bool =
  try
    let parts = String.split_on_char '/' s in
    if List.length parts <> 3 then false
    else
      let month = int_of_string @@ List.nth parts 0 in
      let day = int_of_string @@ List.nth parts 1 in
      let year = int_of_string @@ List.nth parts 2 in
      month >= 1 && month <= 12 && day >= 1
      && (day
         <=
         match month with
         | 2 ->
             if year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) then
               29
             else 28
         | 4 | 6 | 9 | 11 -> 30
         | _ -> 31)
      && year >= 0 && year <= 99
  with _ -> false

let post (user_id : int) (username : string) (feed : f) =
  if user_id <> 0 then (
    print_string "\nPlease enter the title of your post:\n";
    let title = read_line () in

    print_string "\nPlease enter the description of your post:\n";
    let description = read_line () in

    print_string
      "\nPlease enter the price of your post in the format \"0.00\":\n";
    let price = read_line () in

    let rec read_date () =
      print_string
        "\nPlease enter the date of your post in the format \"MM/DD/YY\":\n";
      let date = read_line () in
      if is_date_format date then date else read_date ()
    in
    let date = read_date () in

    let existing_json =
      try Yojson.Basic.from_file file_path
      with _ -> `Assoc [ ("listings", `List []) ]
    in
    let existing_feed = feed_from_json existing_json in

    let get_max_id feed =
      List.fold_left (fun acc listing -> max acc listing.listing_id) 0 feed
    in

    let new_listing =
      {
        listing_id = get_max_id existing_feed.feed + 1;
        user_id;
        username;
        title;
        description;
        price;
        date;
        likes = 0;
      }
    in
    let updated_feed = { feed = existing_feed.feed @ [ new_listing ] } in
    save_to_json updated_feed;

    print_string "\nPost created successfully!\n")
  else print_string "\nYou need to sign in to create a post.\n"
