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
val like_post : int -> f -> unit
val get_title : listing -> string
