open Core

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
  let users, transactions = Fileio.load () in
  match (Set.mem users src, Set.mem users dest) with
  | false, _ -> failwith ("User " ^ src ^ " does not exist")
  | _, false -> failwith ("User " ^ dest ^ " does not exist")
  | _ -> (
      Stdio.print_endline ("Transferred " ^ amt ^ " from " ^ src ^ " to " ^ dest);
      match Map.find transactions (src, dest) with
      | None ->
          transactions
          |> Map.add_exn ~key:(src, dest) ~data:(Money.create amt)
          |> Fileio.save users
      | Some old_amt ->
          let transactions = Map.remove transactions (src, dest) in
          transactions
          |> Map.add_exn ~key:(src, dest)
               ~data:(Money.add old_amt (Money.create amt))
          |> Fileio.save users)

let check usr () =
  let users, transactions = Fileio.load () in
  (match Set.mem users usr with
  | false -> failwith "User does not exist"
  | true -> ());
  let find_transaction src dest =
    match Map.find transactions (src, dest) with
    | None -> Money.create "0"
    | Some amt -> amt
  in
  let process src dest =
    match String.equal src dest with
    | true -> ()
    | false -> (
        let owes = find_transaction src dest in
        let owed = find_transaction dest src in
        match Money.equal owes owed with
        | true -> ()
        | false ->
            if Money.gt owes owed then
              Stdio.print_endline
                (src ^ " owes " ^ dest ^ " "
                ^ Money.to_string (Money.sub owes owed))
            else
              Stdio.print_endline
                (src ^ " is owed " ^ dest ^ " "
                ^ Money.to_string (Money.sub owed owes)))
  in

  Set.iter users ~f:(fun dest -> process usr dest)

let add usr () =
  let users, transactions = Fileio.load () in
  Stdio.print_endline ("Adding user " ^ usr);
  if Bool.equal (complete_match usr username_regex) false then
    failwith "Invalid syntax for username, only alphabetic characters allowed";
  (match Set.mem users usr with
  | true -> failwith "User already exists"
  | false -> ());
  Fileio.save (Set.add users usr) transactions;
  Stdio.print_endline ("Added user " ^ usr)

let delete usr () =
  let users, transactions = Fileio.load () in
  Stdio.print_endline ("Deleting user " ^ usr);
  (match Set.mem users usr with
  | false -> failwith "Cannot delete user that does not exist"
  | true -> ());
  let users = Set.remove users usr in
  let transactions =
    Map.filter_keys transactions ~f:(fun (src, dest) ->
        Bool.equal (String.equal src usr) false
        && Bool.equal (String.equal dest usr) false)
  in
  Fileio.save users transactions;
  Stdio.print_endline ("User " ^ usr ^ " deleted")
