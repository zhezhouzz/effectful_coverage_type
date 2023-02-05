open Locks_sig
open Effect
open Effect.Deep
open Utils

type _ Effect.t +=
  | MkAction : int -> unit t
  | MkLock : int -> unit t
  | MkUnlock : int -> unit t
  | MkNewLock : int -> unit t
  | ShowLocks : int list t

let rec gen_s n =
  if n == 0 then ()
  else (
    perform (MkNewLock n);
    gen_s (n - 1))

let rec gen_c n =
  let locks = perform ShowLocks in
  if List.length locks >= n then ()
  else
    let id = perform RandomInt in
    if perform RandomBool then (
      perform (MkNewLock id);
      gen_c n)
    else if perform RandomBool then perform (MkLock id)
    else perform (MkUnlock id);
    gen_c n

let rec gen_sc n =
  let locks = perform ShowLocks in
  if List.length locks >= n then ()
  else
    let id = perform RandomInt in
    (if perform RandomBool || List.length locks == 0 then
     if List.exists (( = ) id) locks then perform (MkAction id)
     else perform (MkNewLock id)
    else
      let idx = Random.int (List.length locks) in
      perform (MkAction (List.nth locks idx)));
    gen_sc n

let handler (main : unit -> unit) =
  let tr = ref [] in
  let locks = Hashtbl.create 100 in
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | MkLock id ->
              Some
                (fun (k : (a, _) continuation) ->
                  let inv =
                    Hashtbl.replace locks id true;
                    TrLock (id, ())
                  in
                  tr := !tr @ [ inv ];
                  continue k ())
          | MkUnlock id ->
              Some
                (fun (k : (a, _) continuation) ->
                  let inv =
                    Hashtbl.replace locks id false;
                    TrUnlock (id, ())
                  in
                  tr := !tr @ [ inv ];
                  continue k ())
          | MkAction id ->
              Some
                (fun (k : (a, _) continuation) ->
                  let inv =
                    if Hashtbl.find locks id then (
                      Hashtbl.replace locks id false;
                      TrUnlock (id, ()))
                    else (
                      Hashtbl.replace locks id true;
                      TrLock (id, ()))
                  in
                  tr := !tr @ [ inv ];
                  continue k ())
          | MkNewLock id ->
              Some
                (fun (k : (a, _) continuation) ->
                  Hashtbl.add locks id false;
                  tr := !tr @ [ TrNewLock (id, ()) ];
                  continue k ())
          | ShowLocks ->
              Some
                (fun (k : (int list, _) continuation) ->
                  continue k (List.of_seq @@ Hashtbl.to_seq_keys locks))
          | _ -> None);
      retc = (fun _ -> !tr);
      exnc = raise;
    }

let tr_gen gen (n : int) = random_handler (fun () -> handler (fun _ -> gen n))

let main gen n () =
  Locks_lib.handler_v1 @@ Locks_lib.interpreter @@ tr_gen gen n
