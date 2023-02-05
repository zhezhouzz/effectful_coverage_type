open Locks_sig
open Effect
open Effect.Deep
open Printf

type _ Effect.t +=
  | NewLock : int -> unit t
  | Lock : int -> unit t
  | Unlock : int -> unit t

let interpreter (tr : invocation list) =
  let step ivc =
    match ivc with
    | TrNewLock (args, res) ->
        let res' = perform (NewLock args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrLock (args, res) ->
        let res' = perform (Lock args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrUnlock (args, res) ->
        let res' = perform (Unlock args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
  in
  fun () -> List.iter step tr

let handler_v1 (main : unit -> unit) =
  let m = Hashtbl.create 100 in
  (* let () = printf "initialized.\n" in *)
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | NewLock id ->
              Some
                (fun (k : (a, _) continuation) ->
                  match Hashtbl.find_opt m id with
                  | Some _ -> failwith "bad NewLock"
                  | None ->
                      Hashtbl.add m id false;
                      continue k ())
          | Lock id ->
              Some
                (fun (k : (a, _) continuation) ->
                  match Hashtbl.find_opt m id with
                  | Some false ->
                      Hashtbl.replace m id true;
                      continue k ()
                  | _ -> failwith "bad lock")
          | Unlock id ->
              Some
                (fun (k : (unit, _) continuation) ->
                  match Hashtbl.find_opt m id with
                  | Some true ->
                      Hashtbl.replace m id false;
                      continue k ()
                  | _ -> failwith "bad unlock")
          | _ -> None);
      retc = (fun _ -> ());
      exnc = raise;
    }

let test () =
  handler_v1 @@ interpreter
  @@ [
       TrNewLock (3, ());
       TrLock (3, ());
       TrNewLock (4, ());
       TrLock (4, ());
       TrUnlock (3, ());
     ]
