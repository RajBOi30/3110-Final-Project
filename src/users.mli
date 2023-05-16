(** Representation of a user that exists in the [users.json] file. *)

type u
(** The type u represents a collection of users. *)

type user
(** The type user represents a user in the system. Contains fields for each
    aspect of a user: user_id, username, password, etc. *)

val users_from_json : Yojson.Basic.t -> u
(** [users_from_json j] takes a [Yojson.Basic.t] representation of users and
    returns a value of type [u]. Requires: [j] is a valid JSON users
    representation.*)

val user_from_json : Yojson.Basic.t -> user
(** [user_from_json] takes a [Yojson.Basic.t] representation of a user and
    returns a value of type [user]. It parses the JSON representation and
    constructs a user with the parsed user data. *)

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
(** [save_post post_number user_id users] takes an integer [post_number], an
    integer [user_id], and a user list [lst], and saves the post with
    post_number to the user's saved posts, identified by user_id. *)

val print_saved_ids : int -> u -> unit
(** [print_saved_ids user_id users] takes an integer [user_id] and a user list
    [users], and prints the saved post ids of the user identified by user_id. *)

val print_saved_posts : int -> u -> unit
(** [print_saved_posts user_id users] takes an integer [user_id] and a user list
    [users], and prints the saved posts of the user identified by user_id. *)

val follow_user : string -> int -> u -> unit
(** [follow_user username user_id users] takes a string [username], an integer
    [user_id], and a user list [users], and makes the user identified by user_id
    follow the user with the given username. *)

val view_following : int -> u -> unit
(** [view_following user_id users] takes an integer [user_id] and a user list
    [users], and prints the list of users that the user identified by user_id is
    following. *)

val create_account : u -> unit
(** [create_account users] takes a user list [users], prompts the user to enter
    a username and password, and creates a new user with the entered
    information. *)

val view_users : u -> unit
(** [view_users users] takes a user list [users] and prints the information of
    all the users. *)

val save_to_users : u -> unit
(** [save_to_users users] takes a user list [users] and saves it to the
    users.json file. *)

val update_user_listings : int -> int -> u -> unit
(** [update_user_listings user_id listing_id users] takes an integer [user_id],
    an integer [listing_id], and a user list [users], and updates the listings
    of the user identified by [user_id] by adding the [listing_id] to their
    listings. *)

val get_following : int -> u -> string list
(** [get_following user_id users] takes an integer [user_id] and a user list
    [users], and returns a list of usernames representing the users that the
    user identified by user_id is following. *)

val to_yojson : user -> Yojson.Basic.t
(** [to_yojson user] takes a user [user] and converts it into a Yojson.Basic.t
    representation. *)

val suggested_user : int -> u -> unit
(** [suggested_user user_id users] takes an integer [user_id] and a user list
    [users], and prints a randomly suggested user that the user identified by
    user_id can follow. *)
