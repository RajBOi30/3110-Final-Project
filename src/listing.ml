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
  {
    listing_id = json |> member "listing id" |> to_int;
    user_id = json |> member "user id" |> to_int;
    username = json |> member "username" |> to_string;
    title = json |> member "title" |> to_string;
    description = json |> member "description" |> to_string;
    price = json |> member "price" |> to_string;
    date = json |> member "date" |> to_string;
    likes = json |> member "likes" |> to_int;
  }

let feed_from_json json : f =
  { feed = json |> member "listings" |> to_list |> List.map listing_from_json }

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
  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" ^ get_title listing ^ "\n"
  ^ "ID: "
  ^ string_of_int (get_listing_id listing)
  ^ "\n" ^ "Item Description: " ^ get_desc listing ^ "\n" ^ "Price: $"
  ^ get_price listing ^ "\n" ^ "Posted by: " ^ get_username listing ^ " on "
  ^ get_date listing ^ "\n" ^ "Likes: "
  ^ string_of_int (get_likes listing)
  ^ "\n"

let rec print_feed acc (lst : f) =
  match lst.feed with
  | [] -> acc
  | [ h ] -> acc ^ single_listing h
  | h :: t -> print_feed (acc ^ single_listing h) { feed = t }

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

let like_post (i : int) (feed : f) =
  let update p =
    if p.listing_id = i then { p with likes = p.likes + 1 } else p
  in
  let new_feed = List.map update feed.feed in
  save_to_json { feed = new_feed }

let post (user_id : int) (username : string) (feed : f) =
  if user_id <> 0 then (
    print_string "\nPlease enter the title of your post:\n";
    let title = read_line () in

    print_string "\nPlease enter the description of your post:\n";
    let description = read_line () in

    print_string
      "\nPlease enter the price of your post in the format \"0.00\":\n";
    let price = read_line () in

    print_string
      "\nPlease enter the date of your post in the format \"DD/MM/YY\":\n";
    let date = read_line () in
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
