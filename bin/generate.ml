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

open Okra
open Cmdliner
module Cal = CalendarLib.Calendar

(* Calendar term *)

let week_term =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"The week of the year defaulting to the current week"
       ~docv:"WEEK" [ "w"; "week" ]

let year_term =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"The year defaulting to the current year" ~docv:"YEAR"
       [ "y"; "year" ]

let calendar_term : Calendar.t Term.t =
  let make week year =
    let week = Option.value ~default:(Cal.week (Cal.now ())) week in
    let year = Option.value ~default:(Cal.year (Cal.now ())) year in
    Calendar.make ~week ~year
  in
  Term.(const make $ week_term $ year_term)

(* Get activity configuration *)
let home =
  match Sys.getenv_opt "HOME" with
  | None -> Fmt.failwith "$HOME is not set!"
  | Some dir -> dir

let default_token_file =
  let ( / ) = Filename.concat in
  home / ".github" / "github-activity-token"

let token =
  Arg.value
  @@ Arg.opt Arg.file default_token_file
  @@ Arg.info
       ~doc:
         "The path to a file containing your github token, defaults to \
          ~/.github/github-activity-token"
       ~docv:"TOKEN" [ "t"; "token" ]

let run cal conf =
  let open Lwt_result.Infix in
  match Lwt_main.run (Activity.run cal conf >|= Fmt.pr "%s") with
  | Ok () -> ()
  | Error (`Msg m) ->
      Fmt.epr "%s" m;
      exit 1

let term =
  let make_with_file cal token_file =
    let token =
      match Get_activity.Token.load token_file with
      | Ok token -> token
      | Error (`Msg msg) ->
          Fmt.epr "%s" msg;
          exit 1
    in
    let conf = Activity.make token in
    run cal conf
  in
  Term.(const make_with_file $ calendar_term $ token)

let cmd =
  let info =
    Term.info "generate" ~doc:"generate a get-activity report"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Produces a markdown document using get-activity of your activity \
             on Github. See the options below for changing things like which \
             week to query for and where to find your token. To generate a \
             token see the README at https://github.com/talex5/get-activity.";
        ]
  in
  (term, info)