(** Representation of a listing that can be manipulated over time.*)

type f
(** The abstract type of the feed. Contains field "feed" which is a list of
    listings. *)

type listing
(** The abstract type of each listing. Contains fields for each aspect of a
    listing: listing_id, user_id, etc. *)

type max_listing_id
(** A mutable integer representing the current highest listing_id. *)

val feed_from_json : Yojson.Basic.t -> f
(** [feed_from_json j] is the market feed that [j] represents. Requires: [j] is
    a valid JSON listings feed representation. *)

val save_to_json : f -> unit
(** [save_to_json f] updates the main listing json file with a new list of
    listings from [f].*)

val print_feed : string -> f -> string
(** [print_feed acc lst] returns a string containing all of the listings from
    [lst.feed] using [single_listing]'s print format. *)

val print_myfeed : int -> string -> f -> string
(** [print_myfeed id acc lst] returns a string containing all of the listings
    made by the user whose user_id is id. *)

val single_listing : listing -> string
(** [single_listing listing] returns a string containing the most relevant
    aspects of a listing in a nice oranized manner. *)

val get_listing : int -> f -> listing
(** [get_listing x lst] returns the listing in lst that has the listing_id
    equivalent to [x]. *)

val get_desc : listing -> string
(** [get_desc x] returns the description associated with listing [x]. *)

val get_title : listing -> string
(** [get_title x] returns the title associated with listing [x]. *)

val get_listing_id : listing -> int
(** [get_listing_id x] returns the listing_id associated with listing [x]. *)

val get_user_id : listing -> int
(** [get_user_id x] returns the user_id associated with listing [x]. *)

val get_username : listing -> string
(** [get_user_id x] returns the username associated with listing [x]. *)

val get_price : listing -> string
(** [get_price x] returns the price associated with listing [x]. *)

val get_date : listing -> string
(** [get_date x] returns the date associated with listing [x]. *)

val get_likes : listing -> int
(** [get_likes x] returns the likes list associated with listing [x]. *)

val get_reviews : listing -> string list
(** [get_reviews x] returns the reviews list associated with listing [x]. *)

val delete_listing : listing -> f -> unit
(** [delete_listing listing feed] updates the main listing json file with a new
    list of listings from [f], without [listing].*)

val archive_listing : listing -> unit
(** [archive_listing listing] updates the main listing json file by changing the
    title and description of [listing] to have (*SOLD!*).*)

val like_post : int -> int -> f -> unit
(** [like_post i user_id feed] updates the listing json file with a new
    (incremented) like count for the listing with listing_id [i]. *)

val post : int -> string -> f -> unit

val is_valid_price : string -> bool
(** [is_valid_price str] returns true if [str] contains only integers before the
    decimal, and only 2 digits after it. e.g is_valid_price 1.99 = true and
    is_valid_price 1.0009 = false. *)

val is_valid_date : string -> bool
(** [is_valid_date str] returns true if the [str] is correctly formatted such
    that the first number is less than 13, second is less than 31 and the third
    is less than 99. There must also be two slashes dividing each integer.*)

val print_reviews : listing -> string
(** [print_reviews listing] returns a string containing all of the reviews for
    [listing] inside the main json file. *)

val add_review : listing -> string -> unit
(** [add_review listing rev] updates the main listing json file by changing the
    reviews for [listing] to include [rev] of listings from [f].*)

val listing_from_json : Yojson.Basic.t -> listing
(** Reads in a single listing from the json file and correctly maps each field
    to a new listing record. *)

val print_feed_by_id : int list -> string -> f -> string
(** [print_feed_by_id id_list acc lst] returns a string containing a feed
    formatted just the same as print_feed, but with only listings whose id is in
    [id_list]. *)

val max_id : int ref
(** A reference to an integer representing the current highest listing_id. *)

val suggested_post : f -> unit
(** [suggested_post lst] retuns a string containing just a single listing that
    is 'suggested.' The suggestion is generated using random number generation. *)
