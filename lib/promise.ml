(*
 * Copyright (c) 2015 OOHASHI Daichi <dico.leque.comicron at gmail.com>,
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the authors nor the names of its contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

type 'a t = 'a node ref ref
and 'a node  =
  | Val of 'a
  | Exn of exn
  | Delayed of (unit -> 'a t)

let (!!) r = ! !r

let rref x = ref (ref x)

let of_val x = rref (Val x)

let delayed f = rref (Delayed f)

let of_fun f =
  delayed (fun () -> of_val (f ()))

let is_evaluated t =
  match !!t with
  | Val _ | Exn _ -> true
  | Delayed _ -> false

let is_val t =
  match !!t with
  | Val _ -> true
  | Exn _ | Delayed _ -> false

let is_exn t =
  match !!t with
  | Exn _ -> true
  | Val _ | Delayed _ -> false

let rec force t =
  match !!t with
  | Val v -> v
  | Exn e -> raise e
  | Delayed f ->
      let p = try f () with exn -> rref (Exn exn) in
      if not (is_evaluated t) then begin
        let content = !t in
        content := !!p;
        p := content
      end;
      force t

let peek t =
  if is_val t then
    Some (force t)
  else
    None

let map f t =
  of_fun (fun () -> f (force t))

let return x =
  of_val x

let bind t f =
  delayed (fun () -> f (force t))

let join t =
  force t
