open Core
open Stringpair

type t = { users : Set.M(String).t; transactions : Money.t Map.M(StringPair).t }

let from_json_strings (users_json, transactions_json) =
  {
    users = Set.m__t_of_sexp (module String) (users_json |> Core.Sexp.of_string);
    transactions =
      Map.m__t_of_sexp
        (module StringPair)
        Money.t_of_sexp
        (transactions_json |> Core.Sexp.of_string);
  }

let to_json_strings tracker =
  ( Set.sexp_of_m__t (module String) tracker.users |> Sexp.to_string,
    Map.sexp_of_m__t (module StringPair) Money.sexp_of_t tracker.transactions
    |> Sexp.to_string )

let transfer amt src dest =
  let tracker = Fileio.load_str () |> from_json_strings in
  match (Set.mem tracker.users src, Set.mem tracker.users dest) with
  | false, _ -> failwith ("User " ^ src ^ " does not exist")
  | _, false -> failwith ("User " ^ dest ^ " does not exist")
  | _ -> (
      match Map.find tracker.transactions (src, dest) with
      | None ->
          {
            tracker with
            transactions =
              tracker.transactions
              |> Map.add_exn ~key:(src, dest) ~data:(Money.create amt);
          }
          |> to_json_strings
      | Some old_amt ->
          {
            tracker with
            transactions =
              (let transactions = Map.remove tracker.transactions (src, dest) in
               transactions
               |> Map.add_exn ~key:(src, dest)
                    ~data:(Money.add old_amt (Money.create amt)));
          }
          |> to_json_strings)

let user_exists user =
  let tracker = Fileio.load_str () |> from_json_strings in
  Set.mem tracker.users user

let get_users =
  let tracker = Fileio.load_str () |> from_json_strings in
  tracker.users |> Set.to_list

let get_transactions src =
  let tracker = Fileio.load_str () |> from_json_strings in
  let query_transaction src dest =
    match Map.find tracker.transactions (src, dest) with
    | None -> Money.create "0"
    | Some amt -> amt
  in
  let process usr =
    (usr, query_transaction src usr, query_transaction usr src)
  in
  let users = tracker.users |> Set.to_list in
  List.map users ~f:process

let add_user usr =
  let tracker = Fileio.load_str () |> from_json_strings in
  match Set.mem tracker.users usr with
  | true -> failwith "User already exists"
  | false ->
      { tracker with users = Set.add tracker.users usr } |> to_json_strings

let delete_user usr =
  let tracker = Fileio.load_str () |> from_json_strings in
  match Set.mem tracker.users usr with
  | false -> failwith "User doesn't exist"
  | true ->
      {
        users = Set.remove tracker.users usr;
        transactions =
          Map.filter_keys tracker.transactions ~f:(fun (src, dest) ->
              Bool.equal (String.equal src usr) false
              && Bool.equal (String.equal dest usr) false);
      }
      |> to_json_strings
