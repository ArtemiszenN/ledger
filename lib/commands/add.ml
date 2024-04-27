open Core

let add usr () =
  Stdio.print_endline ("Adding user " ^ usr);
  if Bool.equal (Regex.complete_match usr Regex.username_regex) false then
    failwith "Invalid syntax for username, only alphabetic characters allowed";
  let users_json, transactions_json = Tracker.add_user usr in
  Fileio.save_from_string users_json transactions_json;
  Stdio.print_endline ("Added user " ^ usr)

let add_command =
  Command.basic ~summary:"Adds a user"
    ~readme:(fun () -> "To add a user $usr, use add $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     add usr)
