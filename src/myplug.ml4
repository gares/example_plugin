(* 1 *)
DECLARE PLUGIN "myplug"

let () = Mltop.add_known_plugin (fun () ->
  Flags.if_verbose Pp.ppnl Pp.(str"myplug 1.0 at your service"))
  "myplug"
;;

(* 2 *)
open Glob_term
open Globnames
open Misctypes
open Evar_kinds
open Decl_kinds
open Names
open Proofview
open Pretyping
open Genarg
open Tacticals.New

(* 3 *)
let underscore = GHole(Loc.ghost, GoalEvar, IntroAnonymous, None)

let lambda id = GLambda(Loc.ghost, Name id, Explicit, underscore, underscore)

let myintro id : unit Proofview.tactic = Goal.nf_enter (fun g ->
  let env = Goal.env g in
  let goal = Goal.concl g in
  Refine.refine (fun sigma ->
    (* 4 *) ise_pretype_gen (default_inference_flags false) env sigma empty_lvar
      (OfType goal)
      (lambda id)
  ))
;;

let myintros ids = tclTHENLIST (List.map myintro ids)

TACTIC EXTEND myplug_intro
| [ "myintro" ident_list(ids) ] -> [ myintros ids ]
END

let myprint name : unit =
  (* 5 *) let reference = Smartlocate.global_with_alias name in
  match reference with
  | ConstRef c ->
     begin match Global.body_of_constant c with
     | Some b -> Pp.(msg_info (Printer.pr_constr b))
     | None -> Errors.errorlabstrm "myplug" Pp.(str "axiom")
     end
  | _ -> Errors.errorlabstrm "myplug" Pp.(str "can't print this")
;;

VERNAC COMMAND EXTEND Myplug_print CLASSIFIED AS QUERY
| [ "Myprint" global(name) ] -> [ myprint name ]
END

(* vim: set filetype=ocaml foldmethod=marker: *)
