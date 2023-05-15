type f
(** The abstract type of the feed. *)

type listing

val feed_from_json : Yojson.Basic.t -> f
(** [feed_from_json j] is the market feed that [j] represents. Requires: [j] is
    a valid JSON listings feed representation. *)

val save_to_json : f -> unit
val print_feed : string -> f -> string
val print_myfeed : int -> string -> f -> string
val single_listing : listing -> string
val get_listing : int -> f -> listing
val get_desc : listing -> string
val like_post : int -> f -> unit
val get_title : listing -> string
val get_listing_id : listing -> int
val get_user_id : listing -> int
val get_username : listing -> string
val get_desc : listing -> string
val get_price : listing -> string
val get_date : listing -> string
val get_likes : listing -> int
val delete_listing : listing -> f -> unit
val archive_listing : listing -> unit
val like_post : int -> int -> f -> unit
val post : int -> string -> f -> unit
val is_valid_price : string -> bool
val is_valid_date : string -> bool
val get_reviews : listing -> string list
val print_reviews : listing -> string
val add_review : listing -> string -> unit
val listing_from_json : Yojson.Basic.t -> listing
val print_feed_by_id : int list -> string -> f -> string