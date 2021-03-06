(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let ht_add_or_sum ht k v =
  match Hashtbl.find_opt ht k with
  | None -> Hashtbl.add ht k v
  | Some x -> Hashtbl.replace ht k (Float.add v x)

let by_ f ?(include_krs = []) t =
  let uppercase_include_krs = List.map String.uppercase_ascii include_krs in
  let result = Hashtbl.create 7 in
  Report.iter
    (fun e ->
      (* only proceed if include_krs is empty or has a match *)
      if
        include_krs = []
        ||
        match e.id with
        | ID id -> List.mem (String.uppercase_ascii id) uppercase_include_krs
        | _ -> false
      then f result e
      else ()) (* skip this KR *)
    t;
  result

let by_engineer =
  by_ (fun result e ->
      Hashtbl.iter (fun k w -> ht_add_or_sum result k w) e.time_per_engineer)

let by_kr =
  by_ (fun result e ->
      let time =
        Hashtbl.fold (fun _ w acc -> acc +. w) e.time_per_engineer 0.
      in
      Hashtbl.add result (e.title, e.id) time)

let by_objective =
  by_ (fun result e ->
      let time =
        Hashtbl.fold (fun _ w acc -> acc +. w) e.time_per_engineer 0.
      in
      ht_add_or_sum result e.objective time)

let by_project =
  by_ (fun result e ->
      let time =
        Hashtbl.fold (fun _ w acc -> acc +. w) e.time_per_engineer 0.
      in
      ht_add_or_sum result e.project time)
