open Core

(**
  Tracks a future transaction of amt from src to dest

  For example, if person A pays for person B's meal, we record a future transaction from B to A since B has to pay A back in the future. 
  
  @param amt Amount of money to be transferred. This should be a string which may or may not contain '$' and may or may not contain 2 decimal places 
  @param src Person who has to transfer money. This should be a string of a user 
  @param dest Person who is to receive money. This should be a string of a user 
  @return Returns unit
*)
let transfer amt src dest () =
  if String.equal src dest then failwith "That literally does nothing :bully:";
  let users_json, transactions_json = Tracker.transfer amt src dest in
  Fileio.save_from_string users_json transactions_json;
  Stdio.print_endline ("Transferred " ^ amt ^ " from " ^ src ^ " to " ^ dest)

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
