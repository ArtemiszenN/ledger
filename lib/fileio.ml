open Base
open Stringpair

let data_path = "data.json"

let create_data_if_not_exist () =
  if
    Poly.equal (Sys_unix.file_exists data_path) `No
    || Poly.equal (Sys_unix.file_exists data_path) `Unknown
  then (
    let oc = Stdio.Out_channel.create data_path in
    Stdio.Out_channel.output_string oc "";
    Stdio.Out_channel.flush oc;
    Stdio.Out_channel.close oc)

let load () =
  let buf = Core.In_channel.read_all data_path in
  if String.compare buf "" > 0 then
    let json = Yojson.Basic.from_string buf in
    let users_json =
      json |> Yojson.Basic.Util.member "users" |> Yojson.Basic.Util.to_string
    in
    let transactions_json =
      json
      |> Yojson.Basic.Util.member "transactions"
      |> Yojson.Basic.Util.to_string
    in
    ( Set.m__t_of_sexp (module String) (users_json |> Core.Sexp.of_string),
      Map.m__t_of_sexp
        (module StringPair)
        Money.t_of_sexp
        (transactions_json |> Core.Sexp.of_string) )
  else (Set.empty (module String), Map.empty (module StringPair))
[@@deprecated "Overexposed module, use load_str instead"]

let load_str () =
  try
    let buf = Core.In_channel.read_all data_path in
    if String.compare buf "" > 0 then
      let json = Yojson.Basic.from_string buf in
      let users_json =
        json |> Yojson.Basic.Util.member "users" |> Yojson.Basic.Util.to_string
      in
      let transactions_json =
        json
        |> Yojson.Basic.Util.member "transactions"
        |> Yojson.Basic.Util.to_string
      in
      (users_json, transactions_json)
    else ("()", "()")
  with Sys_error _ -> ("()", "()")

let save users transactions =
  let savefile =
    `Assoc
      [
        ( "users",
          `String (Set.sexp_of_m__t (module String) users |> Sexp.to_string) );
        ( "transactions",
          `String
            (Map.sexp_of_m__t (module StringPair) Money.sexp_of_t transactions
            |> Sexp.to_string) );
      ]
  in
  Yojson.Basic.to_file data_path savefile
[@@deprecated "Overexposed module, use save_from_string instead"]

let save_from_string users_str transactions_str =
  let savefile =
    `Assoc
      [
        ("users", `String users_str); ("transactions", `String transactions_str);
      ]
  in
  Yojson.Basic.to_file data_path savefile
