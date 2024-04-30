let main () =
  Fileio.create_data_if_not_exist ();
  Command_unix.run ~version:"1.000000001a" Commands.command
