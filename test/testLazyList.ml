open OUnit2
module L = LazyList

module Option = struct
  let get = function
    | Some x -> x
    | None -> invalid_arg "none"

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let bind f = function
    | Some x -> f x
    | None -> None
end

let test_timeout name f =
  name>: test_case ~length:(OUnitTest.Custom_length 1.0) f

let test_hd =
  "hd">:: fun ctx ->
    assert_equal (Some 1) @@ L.hd @@ L.cons 1 @@ L.nil ();
    assert_equal None @@ L.hd @@ L.nil ();
    ()

let test_tl =
  "tl">:: fun ctx ->
    assert_equal None @@ L.tl @@ L.nil ();
    assert_equal (Some 2) @@ Option.bind L.hd @@ L.tl @@ L.cons 1 @@ L.cons 2 @@ L.nil ();
    ()

let test_to_list =
  "to_list">:: fun ctx ->
    assert_equal [] @@ L.to_list @@ L.nil ();
    assert_equal [1; 2] @@ L.to_list @@ L.cons 1 @@ L.cons 2 @@ L.nil ();
    ()

let test_rev_infinite =
  "rev <infinite-list> should terminate">:: fun ctx ->
    ignore @@ L.rev @@ L.repeat 0

let test_append_infinite =
  "append infinite-lists should terminate">:: fun ctx ->
    ignore @@ L.append (L.repeat 0) @@ L.repeat 1

let big_number = 1_000_000

let test_filter_map_infinite_hd =
  "filter_map infinite hd">:: fun ctx ->
    ignore @@ L.hd @@ L.filter_map (fun x -> if x > big_number then Some x else None) @@ L.from 0

let test_concat_map_infinite_hd =
  "concat_map infinite hd">:: fun ctx ->
    ignore @@ L.hd @@ L.concat_map L.from @@ L.from 0

let test_exists_infinite_hd =
  "exists is tail-recursive">:: fun ctx ->
    ignore @@ L.exists (fun x -> x > big_number) @@ L.from 0

let test_for_all_infinite_hd =
  "for_all is tail-recursive">:: fun ctx ->
    ignore @@ L.for_all (fun x -> x < big_number) @@ L.from 0

let test_intersperse_infinite_hd =
  "intersperse hd infinite">:: fun ctx ->
    ignore @@ L.hd @@ L.intersperse 0 @@ L.from 1

let test_intercalate_infinite_hd =
  "intercalate hd infinite">:: fun ctx ->
    ignore @@ L.hd @@ L.intercalate (L.from 0 ~until:10) @@ L.map L.from @@ L.from 1

let test_filter_infinite_hd =
  "filter infinite hd">:: fun ctx ->
    ignore @@ L.hd @@ L.filter (fun x -> x > big_number) @@ L.from 0

let test_filter_infinite_timeout =
  test_timeout "filter infinite timeout" @@ fun ctx ->
  ignore @@ L.hd @@ L.filter (fun x -> x < -1) @@ L.from 0

let test_partition_infinite_hd =
  "partition infinite hd">:: fun ctx ->
    let xs, ys = L.partition (fun x -> x > big_number) @@ L.from 0 in
    ignore @@ L.hd xs;
    ignore @@ L.hd ys;
    ()

let test_partition_infinite_timeout_fst =
  test_timeout "partition infinite timeout fst" @@ fun ctx ->
  ignore @@ L.length @@ fst @@ L.partition (fun x -> x mod 2 = 0) @@ L.from 0

let test_partition_infinite_timeout_snd =
  test_timeout "partition infinite timeout snd" @@ fun ctx ->
  ignore @@ L.length @@ snd @@ L.partition (fun x -> x mod 2 <> 0) @@ L.from 0

let test_split_at_infinite_hd =
  "split_at infinite hd">:: fun ctx ->
    let xs, ys = L.split_at big_number @@ L.from 0 in
    ignore @@ L.hd xs;
    ignore @@ L.hd ys;
    ()

let test_split_at_infinite_length =
  "split_at infinite length">:: fun ctx ->
    let xs, _ys = L.split_at big_number @@ L.from 0 in
    ignore @@ L.length xs;
    ()

let test_split_at'_infinite_hd =
  "split_at' infinite hd">:: fun ctx ->
    let xs, ys = L.split_at' big_number @@ L.from 0 in
    ignore @@ List.hd xs;
    ignore @@ L.hd ys;
    ()

let test_span_infinite_hd =
  "span infinite hd">:: fun ctx ->
    let xs, ys = L.span (fun x -> x < big_number) @@ L.from 0 in
    ignore @@ L.hd xs;
    ignore @@ L.hd ys;
    ()

let test_span_infinite_length =
  "span infinite length">:: fun ctx ->
    let xs, ys = L.span (fun x -> x < big_number) @@ L.from 0 in
    ignore @@ L.length xs;
    ()

let test_cycle_infinite_hd =
  "span infinite hd">:: fun ctx ->
    ignore @@ L.hd @@ L.cycle @@ L.from 0

let test_unzip_infinite_hd =
  "span infinite hd">:: fun ctx ->
    let xs, ys = L.unzip @@ L.zip (L.from 0) (L.from 1) in
    ignore @@ L.hd xs;
    ignore @@ L.hd ys;
    ()

(** arbitrary finite lazy lists *)
let lazy_list gen =
  QCheck.(map ~rev:L.to_list L.of_list (list gen))

let non_empty_lazy_list gen =
  QCheck.(map ~rev:L.to_list L.of_list
          @@ list_of_size (Gen.int_range 1 128) gen)

let test_of_list_to_list_id =
  QCheck.Test.make
    ~name:"(of_list >>> to_list) is identity"
    QCheck.(list int)
    (fun xs ->
       xs = L.to_list @@ L.of_list xs)

let test_is_empty =
  QCheck.Test.make
    ~name:"is_empty"
    QCheck.(list int)
    (fun xs ->
       (xs = []) = L.is_empty @@ L.of_list xs)

let test_of_list_equal_same =
  QCheck.Test.make
    ~name:"forall 'a (xs : 'a list), equal (of_list xs) (of_list xs)"
    QCheck.(list int)
    (fun xs ->
       L.equal (L.of_list xs) (L.of_list xs))

let test_of_list_equal =
  QCheck.Test.make
    ~name:"forall 'a (xs ys : 'a list), (xs = ys) <-> equal (of_list xs) (of_list ys)"
    QCheck.(pair (list int) (list int))
    (fun (xs, ys) ->
       (xs = ys) = L.equal (L.of_list xs) (L.of_list ys))

let test_hd_cons =
  QCheck.Test.make
    ~name:"cons & hd"
    QCheck.(pair int (lazy_list int))
    (fun (x, xs) ->
       Some x = L.hd @@ L.cons x @@ xs)

let test_tl_cons =
  QCheck.Test.make
    ~name:"cons & tl"
    QCheck.(pair int (lazy_list int))
    (fun (x, xs) ->
       L.equal xs @@ Option.get @@ L.tl @@ L.cons x @@ xs)

let test_nth =
  QCheck.Test.make
    ~name:"nth"
    QCheck.(pair small_int (list int))
    (fun (n, xs) ->
       let lis = L.of_list xs in
       if n < List.length xs then
         L.nth lis n = Some (List.nth xs n)
       else
         L.nth lis n = None)

let test_length =
  QCheck.Test.make
    ~name:"length"
    QCheck.(list int)
    (fun xs ->
       L.length @@ L.of_list xs = List.length xs)

let test_length_greater_than =
  QCheck.Test.make
    ~name:"length_greater_than"
    QCheck.(pair small_int (lazy_list int))
    (fun (n, xs) ->
       (L.length xs > n) = (L.length_greater_than n xs))

let test_cons_length =
  QCheck.Test.make
    ~name:"cons >>> length"
    QCheck.(pair int (lazy_list int))
    (fun (x, xs) ->
       L.length @@ L.cons x xs = L.length xs + 1)

let test_tl_length =
  QCheck.Test.make
    ~name:"tl >>> length"
    QCheck.(non_empty_lazy_list int)
    (fun xs ->
       L.length @@ Option.get @@ L.tl xs = L.length xs - 1)

let test_map_length =
  QCheck.Test.make
    ~name:"map >>> length"
    QCheck.(pair (fun1 int float) (lazy_list int))
    (fun (f, xs) ->
       L.length @@ L.map f xs = L.length xs)

let test_filter_length =
  QCheck.Test.make
    ~name:"filter >>> length"
    QCheck.(pair (fun1 int bool) (lazy_list int))
    (fun (f, xs) ->
       L.length @@ L.filter f xs <= L.length xs)

let test_filter_map_length =
  QCheck.Test.make
    ~name:"filter_map >>> length"
    QCheck.(pair (fun1 int (option string)) (lazy_list int))
    (fun (f, xs) ->
       L.length @@ L.filter_map f xs <= L.length xs)

let test_zip_with_length =
  QCheck.Test.make
    ~name:"zip_with >>> length"
    QCheck.(pair (lazy_list int) (lazy_list int))
    (fun (xs, ys) ->
       let f x y = () in
       L.length (L.zip_with f xs ys) = min (L.length xs) (L.length ys))

let test_rev_append_length =
  QCheck.Test.make
    ~name:"rev_append >>> length"
    QCheck.(pair (lazy_list int) (lazy_list int))
    (fun (xs, ys) ->
       L.length (L.rev_append xs ys) = L.length xs + L.length ys)

let test_rev_length =
  QCheck.Test.make
    ~name:"rev >>> length"
    QCheck.(lazy_list int)
    (fun xs ->
       L.length (L.rev xs) = L.length xs)

let test_rev_rev_id =
  QCheck.Test.make
    ~name:"rev >>> rev"
    QCheck.(lazy_list int)
    (fun xs ->
       L.equal xs @@ L.rev @@ L.rev xs)

let test_append_length =
  QCheck.Test.make
    ~name:"append >>> length"
    QCheck.(pair (lazy_list int) (lazy_list int))
    (fun (xs, ys) ->
       L.length (L.append xs ys) = L.length xs + L.length ys)

let test_take_length =
  QCheck.Test.make
    ~name:"take >>> length"
    QCheck.(pair small_int (lazy_list int))
    (fun (n, xs) ->
       let len = L.length xs in
       let res = L.take n xs in
       if n <= len then
         L.length res = n
       else
         L.length res < n)

let test_drop_length =
  QCheck.Test.make
    ~name:"drop >>> length"
    QCheck.(pair small_int (lazy_list int))
    (fun (n, xs) ->
       let len = L.length xs in
       let res = L.drop n xs in
       if n <= len then
         L.length res = len - n
       else
         L.length res = 0)

let test_take_while =
  QCheck.Test.make
    ~name:"take_while"
    QCheck.(pair (fun1 int bool) (lazy_list int))
    (fun (p, xs) ->
       let ls = L.take_while p xs in
       L.for_all p ls
       && L.length ls <= L.length xs)

let test_drop_while =
  QCheck.Test.make
    ~name:"drop_while"
    QCheck.(pair (fun1 int bool) (lazy_list int))
    (fun (p, xs) ->
       let ls = L.drop_while p xs in
       L.is_empty @@ L.take_while p ls
       && L.length ls <= L.length xs)

let test_span =
  QCheck.Test.make
    ~name:"span"
    QCheck.(pair (fun1 int bool) (lazy_list int))
    (fun (p, xs) ->
       let l1, l2 = L.span p xs in
       L.for_all p l1
       && L.is_empty @@ L.take_while p l2
       && L.length l1 + L.length l2 = L.length xs)

let test_break =
  QCheck.Test.make
    ~name:"break"
    QCheck.(pair (fun1 int bool) (lazy_list int))
    (fun (p, xs) ->
       let l1, l2 = L.break p xs in
       let not_p x = not @@ p x in
       L.for_all not_p l1
       && L.is_empty @@ L.take_while not_p l2
       && L.length l1 + L.length l2 = L.length xs)

let test_intersperse_length =
  QCheck.Test.make
    ~name:"intersperse >>> length"
    QCheck.(pair int (lazy_list int))
    (fun (sep, xs) ->
       let len = L.length xs in
       let res = L.length @@ L.intersperse sep xs in
       if len = 0 then
         res = 0
       else
         res = 2 * len - 1)

let test_from_pos_step =
  QCheck.Test.make
    ~name:"from (step > 0)"
    QCheck.(triple small_int (int_range 1 128) small_int)
    (fun (start, step, until) ->
       let res = L.from start ~step ~until in
       if until < start then
         L.is_empty res
       else
         L.for_all ((=) (-step)) @@ L.zip_with (-) res @@ L.drop 1 res)

let test_from_neg_step =
  QCheck.Test.make
    ~name:"from (step < 0)"
    QCheck.(triple small_int (int_range (-128) (-1)) small_int)
    (fun (start, step, until) ->
       let res = L.from start ~step ~until in
       if start < until then
         L.is_empty res
       else
         L.for_all ((=) (-step)) @@ L.zip_with (-) res @@ L.drop 1 res)

let test_from_zero_step =
  QCheck.Test.make
    ~name:"from (step = 0)"
    QCheck.(pair small_int small_int)
    (fun (start, until) ->
       let res = L.from start ~step:0 ~until in
       L.for_all ((=) start) @@ L.take 100 res)

let test_split_at_length =
  QCheck.Test.make
    ~name:"split_at >>> append >>> length"
    QCheck.(pair small_int (lazy_list int))
    (fun (n, xs) ->
       let ys, zs = L.split_at n xs in
       L.length @@ L.append ys zs = L.length xs)

let test_split_at'_length =
  QCheck.Test.make
    ~name:"split_at >>> append >>> length"
    QCheck.(pair small_int (lazy_list int))
    (fun (n, xs) ->
       let ys, zs = L.split_at' n @@ xs in
       L.length @@ L.append (L.of_list ys) zs = L.length xs)

let test_repeat =
  QCheck.Test.make
    ~name:"repeat"
    QCheck.(pair small_int int)
    (fun (n, x) ->
       let ls = L.repeat x in
       L.nth ls n = Some x)

let test_iterate =
  QCheck.Test.make
    ~name:"iterate"
    QCheck.(triple small_int (fun1 int int) int)
    (fun (n, f, x) ->
       let ls = L.iterate f x in
       L.hd ls = Some x &&
       L.nth ls (n + 1) = Option.map f @@ L.nth ls n)

let test_cycle =
  QCheck.Test.make
    ~name:"cycle"
    QCheck.(non_empty_lazy_list int)
    (fun xs ->
       L.equal xs @@ L.take (L.length xs) @@ L.cycle xs)

let test_cycle_nth =
  QCheck.Test.make
    ~name:"cycle_nth"
    QCheck.(pair small_int (non_empty_lazy_list int))
    (fun (n, xs) ->
       L.nth (L.cycle xs) n = L.nth xs (n mod L.length xs))

let test_is_cyclic =
  QCheck.Test.make
    ~name:"cycle"
    QCheck.(non_empty_lazy_list int)
    (fun xs ->
       L.is_cyclic @@ L.cycle xs)

let test_cycle_is_prefix =
  QCheck.Test.make
    ~name:"cycle_is_prefix"
    QCheck.(non_empty_lazy_list int)
    (fun xs ->
       L.is_prefix xs @@ L.cycle xs)

let test_cycle_drop_is_prefix =
  QCheck.Test.make
    ~name:"cycle_drop_is_prefix"
    QCheck.(pair small_int (non_empty_lazy_list int))
    (fun (n, xs) ->
       L.is_prefix xs @@ L.drop (n * L.length xs) @@ L.cycle xs)

let test_append_is_prefix =
  QCheck.Test.make
    ~name:"append_is_prefix"
    QCheck.(pair (lazy_list int) (lazy_list int))
    (fun (xs, ys) ->
       L.is_prefix xs @@ L.append xs ys)

let suite =
  "LazyList">::: begin
    [ test_hd
    ; test_tl
    ; test_to_list
    ; test_rev_infinite
    ; test_append_infinite
    ; test_filter_map_infinite_hd
    ; test_concat_map_infinite_hd
    ; test_exists_infinite_hd
    ; test_for_all_infinite_hd
    ; test_intersperse_infinite_hd
    ; test_intercalate_infinite_hd
    ; test_filter_infinite_hd
    ; test_filter_infinite_timeout
    ; test_partition_infinite_hd
    ; test_partition_infinite_timeout_fst
    ; test_partition_infinite_timeout_snd
    ; test_split_at_infinite_hd
    ; test_split_at_infinite_length
    ; test_split_at'_infinite_hd
    ; test_span_infinite_hd
    ; test_span_infinite_length
    ; test_cycle_infinite_hd
    ; test_unzip_infinite_hd
    ]
    @ QCheck_runner.to_ounit2_test_list
      [ test_of_list_to_list_id
      ; test_is_empty
      ; test_of_list_equal_same
      ; test_of_list_equal
      ; test_hd_cons
      ; test_tl_cons
      ; test_nth
      ; test_length
      ; test_length_greater_than
      ; test_cons_length
      ; test_tl_length
      ; test_map_length
      ; test_filter_length
      ; test_filter_map_length
      ; test_zip_with_length
      ; test_rev_append_length
      ; test_rev_length
      ; test_rev_rev_id
      ; test_append_length
      ; test_take_length
      ; test_drop_length
      ; test_take_while
      ; test_drop_while
      ; test_span
      ; test_break
      ; test_intersperse_length
      ; test_from_pos_step
      ; test_from_neg_step
      ; test_from_zero_step
      ; test_split_at_length
      ; test_split_at'_length
      ; test_repeat
      ; test_iterate
      ; test_cycle
      ; test_cycle_nth
      ; test_cycle_is_prefix
      ; test_cycle_drop_is_prefix
      ; test_append_is_prefix
      ]
  end
