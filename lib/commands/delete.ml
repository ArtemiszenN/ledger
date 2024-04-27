open Core

let delete usr () =
  Stdio.print_endline ("Deleting user " ^ usr);
  let users_json, transactions_json = Tracker.delete_user usr in
  Fileio.save_from_string users_json transactions_json;
  Stdio.print_endline ("User " ^ usr ^ " deleted")

let delete_command =
  Command.basic ~summary:"Deletes a user"
    ~readme:(fun () -> "To delete a user $usr, use delete $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     delete usr)
