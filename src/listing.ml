open Yojson.Basic.Util

type listing = {
  listing_id : int;
  user_id : int;
  username : string;
  title : string;
  description : string;
  price : float;
  date : string;
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
    price = json |> member "price" |> to_float;
    date = json |> member "date" |> to_string;
  }

let feed_from_json json : f =
  { feed = json |> member "listings" |> to_list |> List.map listing_from_json }

(**[get_listing_id x] returns the listing id of listing [x].*)
let get_listing_id x = x.listing_id

(**[get_user_id x] returns the user id of listing [x].*)
let get_user_id x = x.user_id

(**[get_username x] returns the user id of listing [x].*)
let get_username x = x.username

(**[get_title] returns the title of listing [x].*)
let get_title x = x.title

(**[get_desc] returns the description of listing [x].*)
let get_desc x = x.description

(**[get_price] returns the price of listing [x].*)
let get_price x = x.price

(**[get_date] returns the date of listing [x].*)
let get_date x = x.date
