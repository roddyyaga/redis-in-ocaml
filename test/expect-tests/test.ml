let%expect_test "addition" =
  Printf.printf "%d\n%!" (2 + 2);
  [%expect {| 4 |}]
