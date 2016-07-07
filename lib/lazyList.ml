type 'a t = 'a node Promise.t
and 'a node =
  | Nil
  | Cons of 'a * 'a t

let (!!) x =
  Promise.force x

let nil () =
  Promise.of_val Nil

let cons p cdr =
  Promise.of_val @@ Cons (p, cdr)

let xcons cdr p =
  cons p cdr

let singleton v =
  cons v @@ nil ()

let uncons xs =
  match !!xs with
  | Nil -> None
  | Cons (x, xs') -> Some (x, xs')

let case xs ~nil ~cons =
  match !!xs with
  | Nil -> nil ()
  | Cons (x, xs') -> cons x xs'

let hd xs =
  match !!xs with
  | Nil -> None
  | Cons (x, _) -> Some x

let tl xs =
  match !!xs with
  | Nil -> None
  | Cons (_, xs') -> Some xs'

let rec init xs =
  Promise.delayed (fun () ->
      match !!xs with
      | Nil -> nil ()
      | Cons (x, xs') ->
          begin match !!xs' with
          | Nil -> singleton x
          | Cons _ -> cons x @@ init xs
          end)

let rec last xs =
  match !!xs with
  | Nil -> None
  | Cons (x, xs') ->
      begin match !!xs' with
      | Nil -> Some x
      | Cons _ -> last xs'
      end

let is_empty xs =
  match !!xs with
  | Nil -> true
  | Cons _ -> false

let rec nth xs n =
  match !!xs with
  | Nil -> None
  | Cons (x, xs') ->
      if n = 0 then
        Some x
      else
        nth xs' @@ n - 1

let length xs =
  let rec loop n ys =
    match !!ys with
    | Nil -> n
    | Cons (_, ys') ->
        loop (n + 1) ys'
  in loop 0 xs

let rec length_greater_than n xs =
  if n < 0 then
    true
  else
    match !!xs with
    | Nil -> false
    | Cons (_, xs') -> length_greater_than (n - 1) xs'

let rec fold_left f init xs =
  Promise.delayed (fun () ->
      match !!xs with
      | Nil -> init
      | Cons (x, xs') ->
          fold_left f (f init x) xs')

let rec fold_left' f init xs =
  match !!xs with
  | Nil -> init
  | Cons (x, xs') ->
      fold_left' f (f init x) xs'

let rec fold_right f xs init =
  Promise.delayed (fun () ->
      match !!xs with
      | Nil -> init
      | Cons (x, xs') ->
          f x @@ fold_right f xs' init)

let rec fold_right' f xs init =
  match !!xs with
  | Nil -> init
  | Cons (x, xs') ->
      f x @@ fold_right' f xs' init

let rec unfold_left f init =
  Promise.delayed (fun () ->
      match f init with
      | None -> nil ()
      | Some (v, init') ->
          cons v @@ unfold_left f init')

let unfold_right f init =
  let rec loop init tl =
    Promise.delayed (fun () ->
        match f init with
        | None -> tl
        | Some (v, init') ->
            loop init' @@ cons v tl)
  in loop init @@ nil ()

let append xs ys =
  fold_right cons xs ys

let concat xss =
  fold_right append xss @@ nil ()

let rev_append xs ys =
  fold_left xcons ys xs

let rev xs =
  rev_append xs @@ nil ()

let map f xs =
  fold_right (fun x ys ->
      cons (f x) ys) xs @@ nil ()

let rec scan_left f init xs =
  Promise.delayed (fun () ->
      let tl =
        match !!xs with
        | Nil -> nil ()
        | Cons (x, xs') ->
            scan_left f (f init x) xs'
      in cons init @@ tl)

let rec iter f xs =
  match !!xs with
  | Nil -> ()
  | Cons (x, xs') ->
      f x;
      iter f xs'

let rec filter_map f xs =
  fold_right (fun x ys -> match f x with
      | None -> ys
      | Some y -> cons y ys) xs @@ nil ()

let concat_map f xs =
  concat (map f xs)

let rec exists p xs =
  match !!xs with
  | Nil -> false
  | Cons (x, xs') ->
      p x || exists p xs'

let rec for_all p xs =
  match !!xs with
  | Nil -> true
  | Cons (x, xs') ->
      p x && for_all p xs'

let rec intersperse sep xs =
  Promise.delayed (fun () ->
      match !!xs with
      | Nil -> xs
      | Cons (x, xs') ->
          begin match !!xs' with
          | Nil -> xs
          | Cons _ ->
              cons x @@ cons sep @@ intersperse sep xs'
          end)

let intercalate xs yss =
  concat (intersperse xs yss)

let filter p xs =
  fold_right (fun x ys ->
      if p x then
        cons x ys
      else
        ys) xs @@ nil ()

let partition p xs =
  let r = map (fun x -> if p x then `Left x else `Right x) xs in
  let get_left = function
    | `Left x -> Some x
    | `Right _ -> None
  in
  let get_right = function
    | `Left _ -> None
    | `Right x -> Some x
  in
  (filter_map get_left r , filter_map get_right r)

let rec zip_with f xs ys =
  Promise.delayed (fun () ->
      match !!xs, !!ys with
      | Nil, _
      | _, Nil -> nil ()
      | Cons (x, xs'), Cons (y, ys') ->
          cons (f x y) @@ zip_with f xs' ys')

let zip xs ys =
  zip_with (fun x y -> (x, y)) xs ys

let unzip xs =
  (map fst xs, map snd xs)

let rec take n xs =
  Promise.delayed (fun () ->
      if n = 0 then
        nil ()
      else
        match !!xs with
        | Nil -> xs
        | Cons (x, xs') ->
            cons x @@ take (n - 1) xs')

let rec drop n xs =
  Promise.delayed (fun () ->
      if n = 0 then
        xs
      else
        match !!xs with
        | Nil -> xs
        | Cons (_, xs') ->
            drop (n - 1) xs')

let rec take_while p xs =
  Promise.delayed (fun () ->
      match !!xs with
      | Nil -> nil ()
      | Cons (x, xs') ->
          if p x then
            cons x @@ take_while p xs'
          else
            nil ())

let rec drop_while p xs =
  Promise.delayed (fun () ->
      match !!xs with
      | Nil -> nil ()
      | Cons (x, xs') ->
          if p x then
            drop_while p xs'
          else
            xs)

let split_at n xs =
  (take n xs, drop n xs)

let split_at' n xs =
  let rec loop n ys zs =
    if n <= 0 then
      (List.rev ys, zs)
    else
      match !!zs with
      | Nil -> (List.rev ys, zs)
      | Cons (z, zs') -> loop (n - 1) (z :: ys) zs'
  in loop n [] xs

let span p xs =
  let ys = Promise.delayed (fun () ->
      match !!xs with
      | Nil -> nil ()
      | Cons (x, xs') ->
          scan_left (fun (b, _) y -> (b && p y, y))
            (p x, x) xs')
  in
  (map snd @@ take_while fst ys, map snd @@ drop_while fst ys)

let break p xs =
  span (fun x -> not @@ p x) xs

let from ?until ?(step = 1) start =
  let continue = match until with
    | Some n ->
        if step > 0 then
          fun i -> i < n
        else
          fun i -> n < i
    | None -> fun _m -> true
  in
  let rec loop i =
    Promise.delayed (fun () ->
        if continue i then
          cons i @@ loop @@ i + step
        else
          nil ())
  in loop start

let rec continually f =
  Promise.delayed (fun () ->
      match f () with
      | Some x -> cons x @@ continually f
      | None -> nil ())

let repeat x =
  let r = ref @@ singleton x in
  r := Promise.delayed (fun () -> cons x !r);
  !r

let rec iterate f x =
  Promise.delayed (fun () ->
      cons x @@ iterate f (f x))

let cycle xs =
  if is_empty xs then
    invalid_arg "empty list"
  else
    let r = ref xs in
    r := Promise.delayed (fun () -> append xs !r);
    !r

let is_cyclic xs =
  let rec loop fast slow =
    if fast == slow then
      true
    else
      match !!fast, !!slow with
      | Nil, _
      | _, Nil -> false
      | Cons (_, fast'), Cons (_, slow') ->
          begin match !!slow' with
          | Nil -> false
          | Cons (_, slow'') -> loop fast' slow''
          end
  in loop xs xs

let rec is_prefix ?(eq = (=)) xs ys =
  match !!xs, !!ys with
  | Nil, _ -> true
  | _, Nil -> false
  | Cons (x, xs'), Cons (y, ys') ->
      eq x y && is_prefix ~eq xs' ys'

let rec equal ?(eq = (=)) xs ys =
  match !!xs, !!ys with
  | Nil, Nil -> true
  | Nil, _
  | _, Nil -> false
  | Cons (x, xs'), Cons (y, ys') ->
      eq x y && equal ~eq xs' ys'

let rec compare ?(cmp = Pervasives.compare) xs ys =
  match !!xs, !!ys with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Cons (x, xs'), Cons (y, ys') ->
      begin match cmp x y with
      | 0 -> compare ~cmp xs' ys'
      | n -> n
      end

let to_list xs =
  fold_right' List.cons xs []

let of_list xs =
  xs |> unfold_left (function
      | [] -> None
      | x::xs' -> Some (x, xs'))

let return x = singleton x

let join xs = concat xs

let bind f xs = concat_map f xs
