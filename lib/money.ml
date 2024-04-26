open Core

type t = { dollars : int; cents : int } [@@deriving sexp]

let compare a b =
  match Int.compare a.dollars b.dollars with
  | 0 -> Int.compare a.cents b.cents
  | x -> x

let create str =
  let rem_first_if_dollar_sign s =
    match Char.equal s.[0] '$' with
    | true -> String.sub s ~pos:1 ~len:(String.length s - 1)
    | false -> s
  in
  let ls =
    str |> rem_first_if_dollar_sign |> String.split_on_chars ~on:[ '.' ]
  in
  match List.length ls with
  | 1 -> { dollars = ls |> List.hd_exn |> Int.of_string; cents = 0 }
  | _ ->
      {
        dollars = ls |> List.hd_exn |> Int.of_string;
        cents = List.nth_exn ls 1 |> Int.of_string;
      }

let add a b =
  {
    dollars = a.dollars + b.dollars + (a.cents + b.cents >= 100 |> Bool.to_int);
    cents = (a.cents + b.cents) % 100;
  }

let sub a b =
  let totalcents = (a.dollars * 100) + a.cents - (b.dollars * 100) - b.cents in
  { dollars = totalcents / 100; cents = totalcents % 100 }

let to_string a =
  if a.cents > 0 then
    Int.to_string a.dollars ^ " dollars and " ^ Int.to_string a.cents ^ " cents"
  else Int.to_string a.dollars ^ " dollars"

let gt a b = compare a b > 0
let equal a b = compare a b = 0
