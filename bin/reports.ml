(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2021 Patrick Ferris <pf341@patricoferris.com>
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

open Okra.Aggregate

let pp_days ppf d =
  if d = 1. then Fmt.string ppf "1 day" else Fmt.pf ppf "%.0f days" d

(** [report_team_md okrs] outputs a team report to stdout.

    [include_krs] only includes this list of KR IDs. Note that this will ignore
    empty KR IDs or KRs marked as "NEW KR" unless specified in the list. If the
    list is empty, all KRs are returned.

    When [show_time_calc] is set, an extra line will be added to the output each
    time the same entry is included in the report with a sum at the end. This is
    useful for showing the intermediate steps when aggreating multiple reports
    that contain the same KR.

    [show_time] shows the time entries [show_engineers] shows the list of
    engineers *)
let report_team_md ?(include_krs = []) ?(show_time = true)
    ?(show_time_calc = true) ?(show_engineers = true) okrs =
  let v =
    List.map Okra.Aggregate.of_weekly (List.of_seq (Hashtbl.to_seq_values okrs))
  in
  let uppercase_include_krs = List.map String.uppercase_ascii include_krs in
  let c_project = ref "" in
  let c_objective = ref "" in
  let c_kr_id = ref "" in
  let c_kr_title = ref "" in
  let ppf = Format.std_formatter in
  let pf fmt = Fmt.pf ppf fmt in
  List.iter
    (fun e ->
      (* only proceed if include_krs is empty or has a match *)
      if List.length include_krs = 0 || List.mem e.kr_id uppercase_include_krs
      then (
        if e.project <> !c_project then (
          pf "\n# %s\n" e.project;
          c_project := e.project)
        else ();
        if e.objective <> !c_objective then (
          pf "\n## %s\n" e.objective;
          c_objective := e.objective)
        else ();
        if e.kr_id <> !c_kr_id || e.kr_title <> !c_kr_title then (
          pf "\n- %s (%s)\n" e.kr_title e.kr_id;
          c_kr_title := e.kr_title;
          c_kr_id := e.kr_id)
        else ();
        if show_engineers then
          if show_time then
            if show_time_calc then (
              (* show time calc + engineers *)
              List.iter (pf "  - + %s") e.time_entries;
              pf "  - = ";
              Hashtbl.iter
                (fun e d -> pf "@%s (%a) " e pp_days d)
                e.time_per_engineer;
              pf "\n")
            else (
              (* show total time for each engineer *)
              pf "  - ";
              let first = ref true in
              Hashtbl.iter
                (fun s v ->
                  if not !first then pf ", " else first := false;
                  pf "@%s (%a)" s pp_days v)
                e.time_per_engineer;
              pf "\n")
          else (
            (* only show engineers, no time *)
            Hashtbl.iter (fun s _ -> pf "  - @%s " s) e.time_per_engineer;
            pf "\n")
        else ();
        (* don't show time or engineers *)
        List.iter
          (fun lines ->
            let work = String.concat "\n    " lines in
            pf "  - %s\n" work)
          e.work)
      else () (* skip this KR *))
    (List.sort Okra.Aggregate.compare v)
