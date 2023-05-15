type u
type user

val users_from_json : Yojson.Basic.t -> u
val id_list : u -> int list
val get_uname_from_id : int -> u -> string
val get_pass_from_id : int -> u -> string
val save_post : int -> int -> u -> unit
val print_saved_ids : int -> u -> unit
val print_saved_posts : int -> u -> unit
val follow_user : string -> int -> u -> unit
val view_following : int -> u -> unit
