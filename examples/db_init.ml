open Db_init_sig
open Effect
open Effect.Deep
open Utils

type _ Effect.t +=
  | MkOpen : (unit * bool) -> unit t
  | MkClose : (unit * unit) -> unit t
  | MkPrepare : (unit * bool) -> unit t
  | MkFinalize : (unit * bool) -> unit t
  | MkStep : (string * bool) -> unit t

let gen_s statements =
  perform (MkOpen ((), true));
  perform (MkPrepare ((), true));
  List.iter (fun st -> perform (MkStep (st, true))) statements;
  perform (MkFinalize ((), true));
  perform (MkClose ((), ()))

let gen_c statements =
  let num_cases = 4 + List.length statements in
  let rec loop n =
    if n == 0 then ()
    else
      let i = perform RandomInt mod num_cases in
      (match i with
      | 0 -> perform (MkOpen ((), perform RandomBool))
      | 1 -> perform (MkClose ((), ()))
      | 2 -> perform (MkPrepare ((), perform RandomBool))
      | 3 -> perform (MkFinalize ((), perform RandomBool))
      | i -> perform (MkStep (List.nth statements (i - 4), perform RandomBool)));
      loop (n - 1)
  in
  loop num_cases

let rec gen_sc_ statements record =
  let open_ () =
    perform (MkOpen ((), true));
    perform (MkPrepare ((), true))
  in
  let close_ () =
    perform (MkFinalize ((), true));
    perform (MkClose ((), ()))
  in
  let rec random_new_one l =
    let idx = perform RandomInt mod List.length statements in
    let st = List.nth statements idx in
    if List.exists (String.equal st) l then random_new_one l else st
  in
  let rec loop record =
    let st = random_new_one record in
    perform (MkStep (st, true));
    let record = st :: record in
    if perform RandomBool || List.length record >= List.length statements then
      record
    else loop record
  in
  if List.length record >= List.length statements then ()
  else (
    open_ ();
    let record = loop record in
    close_ ();
    gen_sc_ statements record)

let gen_sc statements = gen_sc_ statements []

let handler (main : unit -> unit) =
  let tr = ref [] in
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | MkOpen (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  tr := !tr @ [ TrOpen (args, res) ];
                  continue k ())
          | MkClose (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  tr := !tr @ [ TrClose (args, res) ];
                  continue k ())
          | MkPrepare (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  tr := !tr @ [ TrPrepare (args, res) ];
                  continue k ())
          | MkFinalize (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  tr := !tr @ [ TrFinalize (args, res) ];
                  continue k ())
          | MkStep (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  tr := !tr @ [ TrStep (args, res) ];
                  continue k ())
          | _ -> None);
      retc = (fun _ -> !tr);
      exnc = raise;
    }

let tr_gen gen n = random_handler (fun () -> handler (fun _ -> gen n))

let main gen n () =
  Db_init_lib.handler @@ Db_init_lib.interpreter @@ tr_gen gen n
