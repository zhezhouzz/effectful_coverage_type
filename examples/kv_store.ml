open Kv_store_sig
open Effect
open Effect.Deep
open Utils

type _ Effect.t +=
  | MkPut : ((int * int) * unit) -> unit t
  | MkGet : (int * int) -> unit t
  | CheckIn : (int * int) -> bool t

let rec gen_sc n =
  let rec gen_next () =
    let key = perform RandomInt in
    let value = perform RandomInt in
    if perform (CheckIn (key, value)) then gen_next ()
    else perform (MkPut ((key, value), ()))
  in
  if n == 0 then ()
  else (
    gen_next ();
    gen_sc (n - 1))

let rec gen_s n =
  if n == 0 then ()
  else (
    perform (MkPut ((n, n), ()));
    gen_s (n - 1))

let rec gen_c n =
  if n == 0 then ()
  else (
    if perform RandomBool then
      perform (MkPut ((perform RandomInt, perform RandomInt), ()))
    else perform (MkGet (perform RandomInt, perform RandomInt));
    gen_c (n - 1))

let handler (main : unit -> unit) =
  let tr = ref [] in
  let keyset = ref IntSet.empty in
  let valueset = ref IntSet.empty in
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | MkGet (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  tr := !tr @ [ TrGet (args, res) ];
                  continue k ())
          | MkPut ((key, value), res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  keyset := IntSet.add key !keyset;
                  valueset := IntSet.add key !valueset;
                  tr := !tr @ [ TrPut ((key, value), res) ];
                  continue k ())
          | CheckIn (key, value) ->
              Some
                (fun (k : (bool, _) continuation) ->
                  continue k
                    (IntSet.mem key !keyset || IntSet.mem value !valueset))
          | _ -> None);
      retc = (fun _ -> !tr);
      exnc = raise;
    }

let tr_gen gen (n : int) = random_handler (fun () -> handler (fun _ -> gen n))

let main gen n () =
  Kv_store_lib.handler_v1 @@ Kv_store_lib.interpreter @@ tr_gen gen n
