open Core

let gen_money =
  let gen_pair =
    Quickcheck.Generator.both (Int.gen_incl 0 2147483647) (Int.gen_incl 0 99)
  in
  let append_0_to_single_char str =
    if String.length str = 1 then "0" ^ str else str
  in
  Quickcheck.Generator.map gen_pair ~f:(fun (dollars, cents) ->
      Ledger.Money.create
        (Int.to_string dollars ^ "."
        ^ (cents |> Int.to_string |> append_0_to_single_char)))

let gen_money_pair = Quickcheck.Generator.both gen_money gen_money

let%test_unit "adding gives positive money" =
  Quickcheck.test gen_money_pair
    ~sexp_of:[%sexp_of: Ledger.Money.t * Ledger.Money.t] ~f:(fun (m, m') ->
      assert (Ledger.Money.add m m' |> Ledger.Money.to_cents >= 0))

let%expect_test "trivial" =
  Stdio.print_endline "Hello World!";
  [%expect {| Hello World! |}]

let%expect_test "money add test" =
  Stdio.print_endline
    (Ledger.Money.to_string
       (Ledger.Money.add
          (Ledger.Money.create "123456.24")
          (Ledger.Money.create "834924.32")));
  [%expect {| 958380 dollars and 56 cents |}]
