type object_phrase = string list

type command =
  | Home
  | Quit
  | MyListing
  | SignIn
  | SignOut
  | Like of int
  | Purchase of int
  | Help
  | Post
  | Reviews
  | Save of int
  | MySaved
  | SavedIDs
  | Follow of string
  | ViewFollowing
  | CreateAccount
  | ViewUsers
  | SuggestedPost
  | SuggestedUser

exception Empty
exception Malformed

(**[input_to_list input] returns a string list of lowercase words without spaces
   in [input].*)
let input_to_list input =
  let lower = String.lowercase_ascii input in
  let lst = String.split_on_char ' ' lower in
  List.filter (fun x -> x <> "") lst

(**[parse input] returns a command given an [input].*)
let parse input =
  match input_to_list input with
  | [] -> raise Empty
  | "home" :: t -> Home
  | "quit" :: t -> Quit
  | "mylistings" :: t -> MyListing
  | "my" :: "listings" :: t -> MyListing
  | "signin" :: t -> SignIn
  | "sign" :: "in" :: t -> SignIn
  | "signout" :: t -> SignOut
  | "sign" :: "out" :: t -> SignOut
  | "help" :: t -> Help
  | "purchase" :: t -> begin
      match t with
      | [ x ] -> ( try Purchase (int_of_string x) with _ -> raise Malformed)
      | [] -> Purchase 0
      | _ -> raise Malformed
    end
  | "post" :: t -> Post
  | "like" :: t -> begin
      match t with
      | [ x ] -> ( try Like (int_of_string x) with _ -> raise Malformed)
      | _ -> raise Malformed
    end
  | "review" :: t | "reviews" :: t -> Reviews
  | "follow" :: t -> begin
      match t with
      | [ x ] -> ( try Follow x with _ -> raise Malformed)
      | _ -> raise Malformed
    end
  | "view" :: "following" :: t -> ViewFollowing
  | "viewfollowing" :: t -> ViewFollowing
  | "createaccount" :: t -> CreateAccount
  | "create" :: "account" :: t -> CreateAccount
  | "viewusers" :: t -> ViewUsers
  | "view" :: "users" :: t -> ViewUsers
  | "save" :: t -> begin
      match t with
      | [ x ] -> ( try Save (int_of_string x) with _ -> raise Malformed)
      | _ -> raise Malformed
    end
  | "my" :: "saved" :: t -> MySaved
  | "saved" :: "ids" :: t -> SavedIDs
  | "mysaved" :: t -> MySaved
  | "savedids" :: t -> SavedIDs
  | "suggestedpost" :: t -> SuggestedPost
  | "suggested" :: "post" :: t -> SuggestedPost
  | "suggesteduser" :: t -> SuggestedUser
  | "suggested" :: "user" :: t -> SuggestedUser
  | _ -> raise Malformed
