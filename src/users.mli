(** Representation of a user that exists in the [users.json] file. *)

type u
type user

val users_from_json : Yojson.Basic.t -> u
val user_from_json : Yojson.Basic.t -> user

val id_list : u -> int list
(** [id_list lst] traverses the user list provided by [u] and returns an integer
    list of user_ids. *)

val get_uname_from_id : int -> u -> string
(** [get_uname_from_id id lst] returns a string containing the username of the
    user with user_id [id].*)

val get_pass_from_id : int -> u -> string
(** [get_pass_from_id id lst] returns a string containing the password of the
    user with user_id [id].*)

val get_id_from_uname : string -> u -> int
(** [get_id_from_uname uname lst] returns an integer representing the id of the
    user with username [uname].*)

val save_post : int -> int -> u -> unit
val print_saved_ids : int -> u -> unit
val print_saved_posts : int -> u -> unit
val follow_user : string -> int -> u -> unit
val view_following : int -> u -> unit
val create_account : u -> unit
val view_users : u -> unit
val save_to_users : u -> unit
val update_user_listings : int -> int -> u -> unit
val get_following : int -> u -> string list
val to_yojson : user -> Yojson.Basic.t
val suggested_user : int -> u -> unit
