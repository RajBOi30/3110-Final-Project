type f
(** The abstract type of the feed. *)

type listing

val feed_from_json : Yojson.Basic.t -> f
(** [feed_from_json j] is the market feed that [j] represents. Requires: [j] is
    a valid JSON listings feed representation. *)

val print_feed : string -> f -> string
