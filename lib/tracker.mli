type t

val from_json_strings : string * string -> t
val to_json_strings : t -> string * string
val transfer : string -> string -> string -> string * string
val user_exists : string -> bool
val get_users : string list
val get_transactions : string -> (string * Money.t * Money.t) list
val add_user : string -> string * string
val delete_user : string -> string * string
