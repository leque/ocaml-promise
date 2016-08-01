open OUnit2

let () =
  run_test_tt_main @@ ("suite" >:::[
      TestPromise.suite
    ])
