open Base

let data_path = "data.json"

module StringPair = struct
  module T = struct
    type t = string * string [@@deriving sexp]

    let compare (a, b) (a', b') =
      match String.compare a a' with 0 -> String.compare b b' | x -> x
  end

  include T
  include Comparator.Make (T)
end

module Money = struct
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
    let flt = str |> rem_first_if_dollar_sign |> Float.of_string in
    { dollars = Int.of_float flt; cents = Int.of_float (flt *. 100.) % 100 }

  let add a b =
    {
      dollars = a.dollars + b.dollars + (a.cents + b.cents >= 100 |> Bool.to_int);
      cents = (a.cents + b.cents) % 100;
    }

  let sub a b =
    let totalcents =
      (a.dollars * 100) + a.cents - (b.dollars * 100) - b.cents
    in
    { dollars = totalcents / 100; cents = totalcents % 100 }

  let to_string a =
    Int.to_string a.dollars ^ " dollars and " ^ Int.to_string a.cents ^ " cents"

  let gt a b = if compare a b >= 0 then true else false
end

let users = ref (Set.empty (module String))
let transactions = ref (Map.empty (module StringPair))

let complete_match str regex =
  let result = Str.string_match regex str 0 in
  let eq = String.equal (Str.matched_string str) str in
  result && eq

let transfer_regex = Str.regexp {|\$?[0-9]*\(\.[0-9][0-9]\)?|}
let username_regex = Str.regexp {|[a-zA-Z]*|}

let transfer amt src dest () =
  if String.equal src dest then failwith "That literally does nothing :bully:";
  if Bool.equal (complete_match amt transfer_regex) false then
    failwith "Invalid syntax for amount specified";
  match (Set.mem !users src, Set.mem !users dest) with
  | false, _ -> failwith ("User " ^ src ^ " does not exist")
  | _, false -> failwith ("User " ^ dest ^ " does not exist")
  | _ -> (
      Stdio.print_endline ("Transferred " ^ amt ^ " from " ^ src ^ " to " ^ dest);
      match Map.find !transactions (src, dest) with
      | None ->
          transactions :=
            Map.add_exn !transactions ~key:(src, dest) ~data:(Money.create amt)
      | Some old_amt ->
          transactions := Map.remove !transactions (src, dest);
          transactions :=
            Map.add_exn !transactions ~key:(src, dest)
              ~data:(Money.add old_amt (Money.create amt)))

let check usr () =
  (match Set.mem !users usr with
  | false -> failwith "User does not exist"
  | true -> ());
  let find_transaction src dest =
    match Map.find !transactions (src, dest) with
    | None -> Money.create "0"
    | Some amt -> amt
  in
  let process src dest =
    match String.equal src dest with
    | true -> ()
    | false ->
        let owes = find_transaction src dest in
        let owed = find_transaction dest src in
        if Money.gt owes owed then
          Stdio.print_endline
            (src ^ " owes " ^ dest ^ " " ^ Money.to_string (Money.sub owes owed))
        else
          Stdio.print_endline
            (src ^ " is owed " ^ dest ^ " "
            ^ Money.to_string (Money.sub owed owes))
  in
  Set.iter !users ~f:(fun dest -> process usr dest)

let add usr () =
  if Bool.equal (complete_match usr username_regex) false then
    failwith "Invalid syntax for username, only alphabetic characters allowed";
  (match Set.mem !users usr with
  | true -> failwith "User already exists"
  | false -> ());
  Stdio.print_endline ("Adding user " ^ usr);
  users := Set.add !users usr;
  Stdio.print_endline (Set.sexp_of_m__t (module String) !users |> Sexp.to_string);
  Stdio.print_endline
    (Map.sexp_of_m__t (module StringPair) Money.sexp_of_t !transactions
    |> Sexp.to_string);
  Stdio.print_endline ("Added user " ^ usr)

let delete usr () =
  Stdio.print_endline ("Deleting user " ^ usr);
  (match Set.mem !users usr with
  | false -> failwith "Cannot delete user that does not exist"
  | true -> ());
  users := Set.remove !users usr;
  transactions :=
    Map.filter_keys !transactions ~f:(fun (src, dest) ->
        Bool.equal (String.equal src usr) false
        && Bool.equal (String.equal dest usr) false);
  Stdio.print_endline ("User " ^ usr ^ " deleted")

let transfer_command =
  Command.basic
    ~summary:"Records a future transaction from one party to another"
    ~readme:(fun () ->
      "To transfer $amt from $a to $b, use transfer $amt $a $b")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map amt = anon ("amt" %: string)
     and src = anon ("src" %: string)
     and dest = anon ("dest" %: string) in
     transfer amt src dest)

let check_command =
  Command.basic ~summary:"Checks information about a user"
    ~readme:(fun () -> "To check information about a user $usr, use check $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     check usr)

let add_command =
  Command.basic ~summary:"Adds a user"
    ~readme:(fun () -> "To add a user $usr, use add $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     add usr)

let delete_command =
  Command.basic ~summary:"Deletes a user"
    ~readme:(fun () -> "To delete a user $usr, use delete $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     delete usr)

let command =
  Command.group ~summary:"MONEYYYY"
    [
      ("transfer", transfer_command);
      ("check", check_command);
      ("add", add_command);
      ("delete", delete_command);
    ]

open Core

let load () =
  let buf = In_channel.read_all data_path in
  if String.compare buf "" > 0 then (
    let json = Yojson.Basic.from_string buf in
    let users_json =
      json |> Yojson.Basic.Util.member "users" |> Yojson.Basic.Util.to_string
    in
    let transactions_json =
      json
      |> Yojson.Basic.Util.member "transactions"
      |> Yojson.Basic.Util.to_string
    in
    users := Set.m__t_of_sexp (module String) (users_json |> Sexp.of_string);
    transactions :=
      Map.m__t_of_sexp
        (module StringPair)
        Money.t_of_sexp
        (transactions_json |> Sexp.of_string))

let save () =
  let savefile =
    `Assoc
      [
        ( "users",
          `String (Set.sexp_of_m__t (module String) !users |> Sexp.to_string) );
        ( "transactions",
          `String
            (Map.sexp_of_m__t (module StringPair) Money.sexp_of_t !transactions
            |> Sexp.to_string) );
      ]
  in
  Yojson.Basic.to_file data_path savefile

let main () =
  load ();
  Command_unix.run ~version:"1.0" command;
  save ()
