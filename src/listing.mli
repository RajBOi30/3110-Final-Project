type f
(** The abstract type of the feed. *)

type listing

val feed_from_json : Yojson.Basic.t -> f
(** [feed_from_json j] is the market feed that [j] represents. Requires: [j] is
    a valid JSON listings feed representation. *)

val save_to_json : f -> unit
val print_feed : string -> f -> string
val print_myfeed : int -> string -> f -> string
val like_post : int -> int -> f -> unit
val post : int -> string -> f -> unit
val print_feed_by_id : int list -> string -> f -> string
