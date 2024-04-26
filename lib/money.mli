type t = { dollars : int; cents : int }

val t_of_sexp : Sexplib0.Sexp.t -> t
val sexp_of_t : t -> Sexplib0.Sexp.t
val compare : t -> t -> int
val create : string -> t
val add : t -> t -> t
val sub : t -> t -> t
val to_string : t -> string
val gt : t -> t -> bool
val equal : t -> t -> bool
