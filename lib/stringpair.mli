open Core

module StringPair : sig
  module T : sig
    type t = string * string

    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val compare : string * string -> string * string -> int
  end

  type t = string * string

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : string * string -> string * string -> int

  type comparator_witness = Base.Comparator.Make(T).comparator_witness

  val comparator : (T.t, comparator_witness) Comparator.t
end
