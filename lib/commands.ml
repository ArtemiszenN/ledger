open Core

let command =
  Command.group ~summary:"MONEYYYY"
    [
      ("transfer", Transfer.transfer_command);
      ("check", Check.check_command);
      ("add", Add.add_command);
      ("delete", Delete.delete_command);
    ]
