open Core

let check usr () =
  (match Tracker.user_exists usr with
  | false -> failwith "User does not exist"
  | true -> ());
  let process (dest, owes, owed) =
    match Money.equal owes owed with
    | true -> ()
    | false ->
        if Money.gt owes owed then
          Stdio.print_endline
            (usr ^ " owes " ^ dest ^ " " ^ Money.to_string (Money.sub owes owed))
        else
          Stdio.print_endline
            (usr ^ " is owed " ^ dest ^ " "
            ^ Money.to_string (Money.sub owed owes))
  in
  List.iter (Tracker.get_transactions usr) ~f:process

let check_command =
  Command.basic ~summary:"Checks information about a user"
    ~readme:(fun () -> "To check information about a user $usr, use check $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     check usr)
