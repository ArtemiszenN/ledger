open Core

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
     Command_handlers.transfer amt src dest)

let check_command =
  Command.basic ~summary:"Checks information about a user"
    ~readme:(fun () -> "To check information about a user $usr, use check $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     Command_handlers.check usr)

let add_command =
  Command.basic ~summary:"Adds a user"
    ~readme:(fun () -> "To add a user $usr, use add $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     Command_handlers.add usr)

let delete_command =
  Command.basic ~summary:"Deletes a user"
    ~readme:(fun () -> "To delete a user $usr, use delete $usr")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map usr = anon ("usr" %: string) in
     Command_handlers.delete usr)

let command =
  Command.group ~summary:"MONEYYYY"
    [
      ("transfer", transfer_command);
      ("check", check_command);
      ("add", add_command);
      ("delete", delete_command);
    ]
