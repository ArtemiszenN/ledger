open Core

module StringPair = struct
  module T = struct
    type t = string * string [@@deriving sexp]

    let compare (a, b) (a', b') =
      match String.compare a a' with 0 -> String.compare b b' | x -> x
  end

  include T
  include Comparator.Make (T)
end
