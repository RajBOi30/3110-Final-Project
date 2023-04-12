type f
(** The abstract type of the feed. *)

type u
type listing
type user

val feed_from_json : Yojson.Basic.t -> f
(** [feed_from_json j] is the market feed that [j] represents. Requires: [j] is
    a valid JSON listings feed representation. *)

val print_feed : string -> f -> string
val users_from_json : Yojson.Basic.t -> u
val id_list : u -> int list
val get_uname_from_id : int -> u -> string
val get_pass_from_id : int -> u -> string
