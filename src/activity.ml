(*
 * Copyright (c) 2021 Patrick Ferris <pf341@patricoferris.com>
 * Copyright (c) 2021 Tim McGilchrist <timmcgil@gmail.com>
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

type project = { title : string; items : string list }

let title { title; _ } = title

type t = { projects : project list; activity : Get_activity.Contributions.t }

let pp_last_week username ppf projects =
  let pp_items ppf t =
    let t = if t = [] then [ "Work Item 1" ] else t in
    Fmt.(pf ppf "%a" (list (fun ppf s -> Fmt.pf ppf "  - %s" s))) t
  in
  let pp_project ppf { title; items } =
    Fmt.pf ppf "- %s\n  - @%s (<X> days)\n%a@." title username pp_items items
  in
  Fmt.pf ppf "%a" Fmt.(list ~sep:(cut ++ cut) pp_project) projects

let repo_org ?(with_id = false) ?(no_links = false) f s =
  let remove_hash s =
    match String.split_on_char '#' s with
    | [ i ] -> i (* Normal PR or Issue ids *)
    | i :: _ -> i (* Extracted from PR reviews *)
    | _ -> failwith "Malformed URL trying to get id number"
  in
  match String.split_on_char '/' s |> List.rev with
  (* URLs with ids have the form (in reverse) <id>/<kind>/<repo>/<org>/... *)
  | i :: _ :: repo :: org :: _ when with_id ->
      if no_links then Fmt.pf f "(%s/%s#%s)" org repo i
      else Fmt.pf f "[%s/%s#%s](%s)" org repo (remove_hash i) s
  (* For now the only kind like this are new repository creations *)
  | repo :: org :: _ -> Fmt.pf f "[%s/%s](%s)" org repo s
  | _ -> Fmt.failwith "Malformed URL %S" s

let pp_ga_item ?(gitlab = false) ~no_links () f
    (t : Get_activity.Contributions.item) =
  match gitlab with
  | true -> (
      match t.kind with
      | `PR -> Fmt.pf f "PR (Gitlab): %s %s" t.title t.url
      | `Issue -> Fmt.pf f "Issue (Gitlab): %s %s" t.title t.url
      | _ -> ())
  | false -> (
      match t.kind with
      | `Issue ->
          Fmt.pf f "Issue: %s %a" t.title
            (repo_org ~with_id:true ~no_links)
            t.url
      | `PR ->
          Fmt.pf f "PR: %s %a" t.title (repo_org ~with_id:true ~no_links) t.url
      | `Review s ->
          Fmt.pf f "%s %s %a" s t.title (repo_org ~with_id:true ~no_links) t.url
      | `New_repo ->
          Fmt.pf f "Created repository %a"
            (repo_org ~with_id:false ~no_links)
            t.url)

let pp_activity ?(gitlab = false) ~no_links () ppf activity =
  let open Get_activity.Contributions in
  let pp_item ppf item =
    Fmt.pf ppf "  - %a" (pp_ga_item ~gitlab ~no_links ()) item
  in
  let bindings = Repo_map.bindings activity in
  let pp_binding ppf (_repo, items) =
    Fmt.pf ppf "%a" Fmt.(list pp_item) items
  in
  Fmt.pf ppf "%a" Fmt.(list pp_binding) bindings

let make ~projects activity = { projects; activity }

let pp ?(gitlab = false) ?(no_links = false) () ppf
    { projects; activity = { username; activity } } =
  Fmt.pf ppf
    {|# Projects

%a

# Last Week

%a
# Activity (move these items to last week)

%a
|}
    Fmt.(list (fun ppf s -> Fmt.pf ppf "- %s" s))
    (List.map title projects) (pp_last_week username) projects
    (pp_activity ~gitlab ~no_links ())
    activity

(* The equivalent get-activity style information for Gitlab
   is in the Events API *)
module Gitlab = struct
  open Get_activity.Contributions

  let make_pull_request url (v : Gitlab_t.event) =
    {
      repo = string_of_int v.event_project_id;
      kind = `PR;
      date = ISO8601.Permissive.string_of_datetime v.event_created_at;
      url;
      title =
        Fmt.str "%a %s"
          Fmt.(option string)
          (Option.map Gitlab_j.string_of_event_action_name v.event_action_name)
          (Option.value ~default:"Gitlab PR" v.event_target_title);
      body = Option.value ~default:"Gitlab PR" v.event_target_title;
    }

  let make_issue url (v : Gitlab_t.event) =
    {
      repo = string_of_int v.event_project_id;
      kind = `Issue;
      date = ISO8601.Permissive.string_of_datetime v.event_created_at;
      url;
      title = Option.value ~default:"Gitlab Issue" v.event_target_title;
      body = Option.value ~default:"Gitlab Issue" v.event_target_title;
    }

  let to_repo_map :
      Gitlab_t.events * (int * Gitlab_t.project_short) list ->
      item list Repo_map.t =
   fun (evs, projects) ->
    let to_ga_item (ev : Gitlab_t.event) =
      let project = List.assoc_opt ev.event_project_id projects in
      match (project, ev.event_target_type) with
      | Some project, Some `MergeRequest ->
          let url =
            Fmt.str "%s/-/merge_requests/%a" project.project_short_web_url
              Fmt.(option int)
              ev.event_target_iid
          in
          Some (make_pull_request url ev)
      | Some project, Some `Issue ->
          let url =
            Fmt.str "%s/-/issues/%a" project.project_short_web_url
              Fmt.(option int)
              ev.event_target_iid
          in
          Some (make_issue url ev)
      | _ -> None
    in
    let lst = List.filter_map to_ga_item evs in
    let repo = Repo_map.empty in
    List.fold_left
      (fun map item ->
        match Repo_map.find_opt "Gitlab" map with
        | Some items -> Repo_map.add "Gitlab" (item :: items) map
        | None -> Repo_map.add "Gitlab" [ item ] map)
      repo lst

  module Fetch
      (Env : Gitlab_s.Env)
      (Time : Gitlab_s.Time)
      (CL : Cohttp_lwt.S.Client) =
  struct
    module G = Gitlab_core.Make (Env) (Time) (CL)
    open G.Monad

    let map_s f l =
      let open G.Monad in
      let rec inner acc = function
        | [] -> List.rev acc |> return
        | hd :: tl -> f hd >>= fun r -> inner (r :: acc) tl
      in
      inner [] l

    let get_project ~token i =
      let+ res = G.Project.by_id ~token ~project_id:i () in
      (i, Option.get @@ G.Response.value res)

    let make_activity ~token ~before ~after =
      let open G.Monad in
      let* res = G.Event.all ~token ~before ~after () in
      let events = G.Response.value res in
      let ids =
        List.map (fun (v : Gitlab_t.event) -> v.event_project_id) events
        |> List.sort_uniq Int.compare
      in
      map_s (get_project ~token) ids >>= fun ids -> return (events, ids)
  end
end
