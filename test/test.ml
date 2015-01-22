open OUnit2

let test_construction ctx =
  let v = 42 in
  let p1 = Promise.of_val v in
  let p2 = Promise.of_fun (fun () -> v) in
  let p3 = Promise.delayed (fun () -> p1) in
  let p4 = Promise.delayed (fun () -> p2) in
  let p5 = Promise.delayed (fun () -> p3) in
  assert_equal v @@ Promise.force p1;
  assert_equal v @@ Promise.force p2;
  assert_equal v @@ Promise.force p3;
  assert_equal v @@ Promise.force p4;
  assert_equal v @@ Promise.force p5

let test_raise ctx =
  let p = Promise.of_fun (fun () -> raise Exit) in
  assert_raises Exit (fun () -> Promise.force p);
  assert_raises Exit (fun () -> Promise.force p)

let test_eval_once ctx =
  let s = "foo" in
  let buf = Buffer.create 16 in
  let p = Promise.of_fun (fun () -> Buffer.add_string buf s) in
  Promise.force p;
  Promise.force p;
  assert_equal s @@ Buffer.contents buf

let test_eval_once2 ctx =
  let s = "foo" in
  let buf = Buffer.create 16 in
  let p = Promise.of_fun (fun () -> Buffer.add_string buf s) in
  let q = Promise.delayed (fun () -> p) in
  let r = Promise.delayed (fun () -> q) in
  Promise.force r;
  Promise.force p;
  assert_equal s @@ Buffer.contents buf

let test_of_val_is_val ctx =
  assert_equal true @@ Promise.is_val @@ Promise.of_val 42

let test_unevaluated_is_not_val ctx =
  let p1 = Promise.of_fun (fun () -> 42) in
  let p2 = Promise.delayed (fun () -> Promise.of_val 42) in
  assert_equal false @@ Promise.is_val p1;
  assert_equal false @@ Promise.is_val p2

let test_forced_is_val ctx =
  let p1 = Promise.of_fun (fun () -> 42) in
  let p2 = Promise.delayed (fun () -> Promise.of_val 42) in
  ignore @@ Promise.force p1;
  ignore @@ Promise.force p2;
  assert_equal true @@ Promise.is_val p1;
  assert_equal true @@ Promise.is_val p2

let ignore_errors f =
  try f () with _ -> ()

let test_exn_is_not_val ctx =
  let p = Promise.of_fun (fun () -> raise Exit) in
  assert_equal false @@ Promise.is_val p;
  ignore_errors (fun () -> Promise.force p);
  assert_equal false @@ Promise.is_val p

let test_evaluated ctx =
  let pv = Promise.of_val 42 in
  let pf = Promise.of_fun (fun () -> 42) in
  let pe = Promise.of_fun (fun () -> raise Exit) in
  assert_equal true @@ Promise.is_evaluated pv;
  assert_equal false @@ Promise.is_evaluated pf;
  assert_equal false @@ Promise.is_evaluated pe;
  ignore @@ Promise.force pv;
  ignore @@ Promise.force pf;
  ignore_errors (fun () -> Promise.force pe);
  assert_equal true @@ Promise.is_evaluated pv;
  assert_equal true @@ Promise.is_evaluated pf;
  assert_equal true @@ Promise.is_evaluated pe

let test_exn ctx =
  let v = 42 in
  let pv = Promise.of_val v in
  let pf = Promise.of_fun (fun () -> v) in
  let pe = Promise.of_fun (fun () -> raise Exit) in
  assert_equal false @@ Promise.is_exn pv;
  assert_equal false @@ Promise.is_exn pf;
  assert_equal false @@ Promise.is_exn pe;
  ignore @@ Promise.force pv;
  ignore @@ Promise.force pf;
  ignore_errors (fun () -> Promise.force pe);
  assert_equal false @@ Promise.is_exn pv;
  assert_equal false @@ Promise.is_exn pf;
  assert_equal true @@ Promise.is_exn pe

let test_peek ctx =
  let v = 42 in
  let pv = Promise.of_val v in
  let pf = Promise.of_fun (fun () -> v) in
  let pe = Promise.of_fun (fun () -> raise Exit) in
  assert_equal (Some 42) @@ Promise.peek pv;
  assert_equal None @@ Promise.peek pf;
  assert_equal None @@ Promise.peek pe;
  ignore @@ Promise.force pv;
  ignore @@ Promise.force pf;
  ignore_errors (fun () -> Promise.force pe);
  assert_equal (Some 42) @@ Promise.peek pv;
  assert_equal (Some 42) @@ Promise.peek pf;
  assert_equal None @@ Promise.peek pe

let test_map ctx =
  let rec iterate n f x =
    if n <= 0 then
      x
    else
      iterate (n - 1) f (f x)
  in
  let n = 4 in
  let p = Promise.of_val 0 in
  p
    |> iterate n (Promise.map succ)
    |> Promise.force
    |> assert_equal n

let test_bind ctx =
  let (>>=) p f = Promise.bind p f in
  let x = 1 in
  let y = 2 in
  let p =
    Promise.of_val x >>= fun i ->
    Promise.of_fun (fun () -> y) >>= fun j ->
    Promise.of_val @@ i + j
  in
  assert_equal (x + y) @@ Promise.force p

module Stream = struct
  type 'a t_ = Nil | Cons of 'a * 'a t
  and 'a t = 'a t_ Promise.t

  let hd t =
    match Promise.force t with
    | Cons (x, _) -> x
    | Nil -> failwith "empty"

  let tl t =
    match Promise.force t with
    | Cons (_, xs) -> xs
    | Nil -> failwith "empty"

  let rec nth n xs =
    if n = 0 then
      hd xs
    else
      nth (n - 1) (tl xs)

  let rec drop n t =
    Promise.delayed (fun () ->
      if n = 0 then
        t
      else
        drop (n - 1) (tl t))

  let rec from n =
    Promise.delayed (fun () ->
      Promise.of_fun (fun () -> Cons (n, from @@ n + 1)))

  let rec traverse s =
    Promise.delayed (fun () ->
      traverse (tl s))

  let rec filter p xs =
    Promise.delayed (fun () ->
      match Promise.force xs with
      | Nil -> Promise.of_val Nil
      | Cons (x, xs') ->
          if p x then
            Promise.of_fun (fun () -> Cons (x, (filter p xs')))
          else
            filter p xs')
end

let memoization_test_1 ctx =
  let buf = Buffer.create 16 in
  let s = Promise.of_fun (fun () ->
    Buffer.add_string buf "hello";
    1)
  in
  ignore @@ Promise.force s;
  ignore @@ Promise.force s;
  assert_equal "hello" @@ Buffer.contents buf

let memoization_test_2 ctx =
  let buf = Buffer.create 16 in
  let s = Promise.of_fun (fun () ->
    Buffer.add_string buf "bonjour";
    2)
  in
  ignore @@ Promise.force s + Promise.force s;
  assert_equal "bonjour" @@ Buffer.contents buf

let memoization_test_3 ctx =
  let buf = Buffer.create 16 in
  let r = Promise.of_fun (fun () ->
    Buffer.add_string buf "hi";
    1)
  in
  let s = Promise.delayed (fun () -> r) in
  let t = Promise.delayed (fun () -> s) in
  ignore @@ Promise.force t;
  ignore @@ Promise.force r;
  assert_equal "hi" @@ Buffer.contents buf

let memoization_test_4 ctx =
  let buf = Buffer.create 16 in
  let rec ones () = Promise.of_fun (fun () ->
    Buffer.add_string buf "ho";
    Stream.Cons (1, ones ()))
  in
  let s = ones () in
  ignore @@ Stream.hd @@ Stream.drop 4 s;
  ignore @@ Stream.hd @@ Stream.drop 4 s;
  assert_equal "hohohohoho" @@ Buffer.contents buf

let reentrancy_test_1 ctx =
  let count = ref 0 in
  let x = ref 0 in
  let p = ref @@ Promise.of_val 0 in   (* dummy *)
  let p' = Promise.of_fun (fun () ->
    incr count;
    if !count > !x then
      !count
    else
      Promise.force !p)
  in
  p := p';
  x := 5;
  assert_equal 6 @@ Promise.force p';
  x := 10;
  assert_equal 6 @@ Promise.force p'

let reentrancy_test_2 ctx =
  let f = ref @@ Promise.of_val "" in
  let f_ =
    let first = ref true in
    Promise.of_fun (fun () ->
      if !first then begin
        first := false;
        Promise.force !f
      end else
        "second")
  in
  f := f_;
  assert_equal "second" @@ Promise.force !f

let reentrancy_test_3 ctx =
  let get_count, p =
    let count = ref 5 in
    let get_count () = !count in
    let p = ref @@ Promise.of_val 0 in  (* dummy *)
    let p' = Promise.of_fun (fun () ->
      if !count <= 0 then
        !count
      else begin
        count := !count - 1;
        ignore @@ Promise.force !p;
        count := !count + 2;
        !count
      end)
    in
    p := p';
    (get_count, !p)
  in
  assert_equal 5 @@ get_count ();
  assert_equal 0 @@ Promise.force p;
  assert_equal 10 @@ get_count ()

(* Tests from SRFI-45 *)

let leak_test_1 ctx =
  let rec loop () = Promise.delayed (fun () -> loop ()) in
  (* NB: Lazy version will cause stack overflow.
  let rec lz () = lazy (Lazy.force (lz ())) in
  ignore @@ Lazy.force @@ lz ();
   *)
  ignore @@ Promise.force @@ loop ()

let leak_test_2 ctx =
  let rec loop () = Promise.delayed (fun () -> loop ()) in
  let s = loop () in
  ignore @@ Promise.force s

let leak_test_3 ctx =
  Stream.from 0 |> Stream.traverse |> Promise.force |> ignore

let leak_test_4 ctx =
  let s = Stream.from 0 |> Stream.traverse in
  ignore @@ Promise.force s

let leak_test_5 ctx =
  Stream.from 0
  |> Stream.filter (fun n -> n = 10000000000)
  |> Promise.force
  |> ignore

let leak_test_6 ctx =
  Stream.from 0
  |> Stream.filter (fun n -> n = 0)
  |> Stream.nth 0
  |> assert_equal 0;
  let s = Stream.from 0 |> Stream.nth 100000000 in
  assert_equal 100000000 s

let leak_test_7 ctx =
  let times3 n =
    Stream.from 0
    |> Stream.filter (fun x -> x mod n = 0)
    |> Stream.nth 3
  in
  ignore @@ times3 7;
  ignore @@ times3 100000000

let suite =
  let open OUnitTest in
  let length = OUnitTest.Short in
  "suite">:::
    [ "test construction">:: test_construction
    ; "test raise">:: test_raise
    ; "test evaluated only once">:: test_eval_once
    ; "test evaluated only once (2)">:: test_eval_once2
    ; "test of_val is val">:: test_of_val_is_val
    ; "test unevaluated is not val">:: test_unevaluated_is_not_val
    ; "test forced is val">:: test_forced_is_val
    ; "test exn is not val">:: test_exn_is_not_val
    ; "test evaluated">:: test_evaluated
    ; "test exn">:: test_exn
    ; "test peek">:: test_peek
    ; "test map">:: test_map
    ; "test bind">:: test_bind
    ; "Memoization test 1">:: memoization_test_1
    ; "Memoization test 2">:: memoization_test_2
    ; "Memoization test 3">:: memoization_test_3
    ; "Memoization test 4">:: memoization_test_4
    ; "Reentrancy test 1">:: reentrancy_test_1
    ; "Reentrancy test 2">:: reentrancy_test_2
    ; "Reentrancy test 3">:: reentrancy_test_3
    ; "Leak test 1 (will tiemout)">: test_case ~length:Immediate leak_test_1
    ; "Leak test 2 (will timeout)">: test_case ~length:Immediate leak_test_2
    ; "Leak test 3 (will timeout)">: test_case ~length:Immediate leak_test_3
    ; "Leak test 4 (will timeout)">: test_case ~length:Immediate leak_test_4
    ; "Leak test 5 (may timeout)">: test_case ~length leak_test_5
    ; "Leak test 6 (may timeout)">: test_case ~length leak_test_6
    ; "Leak test 7 (may timeout)">: test_case ~length leak_test_7
    ]

let () =
  run_test_tt_main suite
