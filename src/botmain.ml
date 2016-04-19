open Lwt.Infix
open Cmdliner
open Printf
open Git
open Git_unix
open Lwt

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Tomas Tauber   <tomtau@hku.hk>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/tomtau/overleafslack/issues.";
]

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

(* Global options *)
type global = {
  verbose: bool;
  color  : bool;
}

let app_global g =
  if g.color then
    Log.color_on ();
  if g.verbose then
    Log.set_log_level Log.DEBUG

let color_tri_state =
  try match Sys.getenv "GITCOLOR" with
    | "always" -> `Always
    | "never"  -> `Never
    | _        -> `Auto
  with
  | Not_found  -> `Auto

let global =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section
        ~doc:"Be more verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let color =
    let doc = Arg.info ~docv:"WHEN"
        ~doc:"Colorize the output. $(docv) must be `always', `never' or `auto'."
        ["color"] in
    let choices = Arg.enum [ "always", `Always; "never", `Never; "auto", `Auto ] in
    let arg = Arg.(value & opt choices color_tri_state & doc) in
    let to_bool = function
      | `Always -> true
      | `Never  -> false
      | `Auto   -> Unix.isatty Unix.stdout in
    Term.(pure to_bool $ arg)
  in
  Term.(pure (fun verbose color -> { verbose; color }) $ verbose $ color)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

let gri =
  let parse str = `Ok (Gri.of_string str) in
  let print ppf name = Format.pp_print_string ppf (Gri.to_string name) in
  parse, print

let remote =
  let doc = Arg.info ~docv:"REPOSITORY"
      ~doc:"Location of the Overleaf repository." [] in
  Arg.(required & pos 0 (some gri) None & doc)

let hookurl =
  let doc = Arg.info ~docv:"HOOKURL"
      ~doc:"Slack Incoming Webhook URL." [] in
  Arg.(required & pos 1 (some string) None & doc)

let channel =
  let doc = Arg.info ~docv:"CHANNEL"
      ~doc:"Slack Channel to Post Notifications to." [] in
  Arg.(required & pos 2 (some string) None & doc)

let checktime =
  let doc = Arg.info ~docv:"CHECKTIME"
      ~doc:"Time period (in seconds) for checking the repository." [] in
  Arg.(required & pos 3 (some int) None & doc)

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun global -> app_global global; fn) $ global)

module ExtractedCommit = struct
  type t = {
    message: string;
    author: string;
    sha1: Git.SHA.Commit.t;
  }
  let message t = t.message
  let author t = t.author
  let sha1 t = t.sha1

  let compare c1 c2 = Git.SHA.Commit.compare c1.sha1 c2.sha1
  let hash c = Git.SHA.Commit.hash c.sha1
  let equal c1 c2 = Git.SHA.Commit.equal c1.sha1 c2.sha1

  module StringSet = Set.Make(String)

  module Set = Set.Make(struct type commit = t
      type t = commit
      let compare = compare
    end)
  let of_git sha1 c =
    let a = Git.Commit.(c.author) in
    let message = Git.Commit.(c.message) in
    let author = Git.User.(a.name) in
    { message; author; sha1 }      

end  

open ExtractedCommit

(* TODO: atdgen *)
let payload channel username text icon_emoji = "{\"channel\": \""^ channel ^"\", \"username\": \"" ^ username ^ "\", \"text\": \"" ^ text ^ "\", \"icon_emoji\": \"" ^ icon_emoji ^ "\"}"

let query_lwt api_url bodypayload =
  Cohttp_lwt_unix.Client.post (Uri.of_string api_url) ~body:bodypayload
  >|= snd
  >>= Cohttp_lwt_body.to_string

let fetch_commits (module S: Store.S) rem =
  let module Sy = Sync.Make(S) in
  S.create ()   >>= fun t ->
  Sy.fetch ~update:true t rem >>= fun c -> 
  (match Sync.Result.head_contents c with
   | Some h -> S.write_head t h
   | None   -> Lwt.return_unit)
  >>= fun _ -> 
  S.read_head t >>= fun head ->
  (match head with
   | Some (Git.Reference.SHA sha) -> return sha
   | Some (Git.Reference.Ref r) -> S.read_reference_exn t r
   | None -> fail(Failure "no head")) >>= fun head ->
  let read_commit_exn t sha =
    S.read_exn t (Git.SHA.of_commit sha) >>= fun v ->
    match v with
    | Git.Value.Commit c -> return c
    | Git.Value.Blob _ | Git.Value.Tag _
    | Git.Value.Tree _ ->
      fail(Failure "not a commit value") in 
  read_commit_exn t head >>= fun commit ->
  let s = ExtractedCommit.Set.add (ExtractedCommit.of_git head commit) ExtractedCommit.Set.empty in
  let dealt = Git.SHA.Commit.Set.(add head empty) in
  let rec get_parent_commits t commit sd =
    let add_parent ((s, dealt) as sd) p_sha =
      if Git.SHA.Commit.Set.mem p_sha dealt then return sd
      else
        let dealt = Git.SHA.Commit.Set.add p_sha dealt in
        read_commit_exn t p_sha >>= fun parent ->
        let s = ExtractedCommit.Set.add (ExtractedCommit.of_git p_sha parent) s
        in
        get_parent_commits t parent (s, dealt)
    in
    Lwt_list.fold_left_s add_parent sd Git.Commit.(commit.parents)
  in
  get_parent_commits t commit (s, dealt) >>= fun coms ->
  Lwt.return (fst coms)

(* TODO: (re-)use Store instead of creating from fresh and passing the set *)
let rec channel_post_loop remote hookurl channel wtime s =
  Lwt_unix.sleep wtime >>= fun () -> let new_s = run (fetch_commits (module Memory: Store.S) remote) in
  let diff_set = ExtractedCommit.Set.diff new_s s in
  Format.printf "new commits: %d@." (ExtractedCommit.Set.cardinal diff_set);
  ExtractedCommit.Set.iter (fun x -> Format.printf "%s@." x.message) new_s; 
  ExtractedCommit.Set.iter (fun x ->
      let _ = run(query_lwt hookurl (Cohttp_lwt_body.of_string (payload channel "overleafbot" (x.author ^ ": " ^ x.message) ":writing_hand:"))) in ()) diff_set;
  channel_post_loop remote hookurl channel wtime new_s

(* post to channel *)
let channel_post = {
  name = "channel_post";
  doc  = "Fetches a remote Git repository and posts its commit messages to a Slack channel.";
  man  = [];
  term = let exec remote hookurl channel checktime =
           let initial = run (fetch_commits (module Memory: Store.S) remote) in
           let _ = channel_post_loop remote hookurl channel (float_of_int checktime) initial in
           Lwt_unix.run (fst(Lwt.wait ()))
    in
    Term.(mk exec $ remote $ hookurl $ channel $ checktime)
}

let default =
  let doc = "Mirage application builder" in
  let man = [
    `S "DESCRIPTION";
    `P "Slack bot for Overleaf -- based on ogit tool"
  ] @  help_sections
  in
  let usage _ =
    Printf.printf
      "usage: olsbot [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
       \n\
       The most commonly used commands are:\n\
      \    channel_post       %s\n\
       \n\
       See 'olsbot help <command>' for more information on a specific command.\n%!"
      channel_post.doc in
  Term.(pure usage $ (pure ())),
  Term.info "olsbot"
    ~version:Version.current
    ~sdocs:global_option_section
    ~doc
    ~man

let () =
  match Term.eval_choice default [command channel_post] with
  | `Error _ -> exit 1
  | _ -> ();

