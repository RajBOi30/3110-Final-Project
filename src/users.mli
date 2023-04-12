type u
type user

val users_from_json : Yojson.Basic.t -> u
val id_list : u -> int list
val get_uname_from_id : int -> u -> string
val get_pass_from_id : int -> u -> string
