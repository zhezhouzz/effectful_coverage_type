open Db_init_sig
open Effect
open Effect.Deep
open Printf

type _ Effect.t +=
  | Open : unit -> bool t
  | Close : unit -> unit t
  | Prepare : unit -> bool t
  | Finalize : unit -> bool t
  | Step : string -> bool t

let interpreter (tr : invocation list) =
  let step ivc =
    match ivc with
    | TrOpen (args, res) ->
        let res' = perform (Open args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrClose (args, res) ->
        let res' = perform (Close args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrPrepare (args, res) ->
        let res' = perform (Prepare args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrFinalize (args, res) ->
        let res' = perform (Finalize args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrStep (args, res) ->
        let res' = perform (Step args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
  in
  fun () -> List.iter step tr

type state = Uninit | Opened | Prepared

let handler (main : unit -> unit) =
  let st = ref Uninit in
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Open () ->
              Some
                (fun (k : (a, _) continuation) ->
                  match !st with
                  | Uninit ->
                      st := Opened;
                      continue k true
                  | _ -> continue k false)
          | Close () ->
              Some
                (fun (k : (a, _) continuation) ->
                  st := Uninit;
                  continue k ())
          | Prepare () ->
              Some
                (fun (k : (bool, _) continuation) ->
                  match !st with
                  | Opened ->
                      st := Prepared;
                      continue k true
                  | _ -> continue k false)
          | Finalize () ->
              Some
                (fun (k : (bool, _) continuation) ->
                  match !st with
                  | Prepared ->
                      st := Opened;
                      continue k true
                  | _ -> continue k false)
          | Step _ ->
              Some
                (fun (k : (bool, _) continuation) ->
                  match !st with
                  | Prepared -> continue k true
                  | _ -> continue k false)
          | _ -> None);
      retc = (fun _ -> ());
      exnc = raise;
    }

let test () =
  handler @@ interpreter
  @@ [
       TrOpen ((), true);
       TrPrepare ((), true);
       TrStep ("SELECT", true);
       TrFinalize ((), true);
       TrClose ((), ());
     ]
