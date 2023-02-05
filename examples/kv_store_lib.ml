open Kv_store_sig
open Effect
open Effect.Deep
open Printf

type _ Effect.t += Put : (int * int) -> unit t | Get : int -> int t

let interpreter (tr : invocation list) =
  let step ivc =
    match ivc with
    | TrPut (args, res) ->
        let res' = perform (Put args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrGet (args, res) ->
        let res' = perform (Get args) in
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
          | Put (key, value) ->
              Some
                (fun (k : (a, _) continuation) ->
                  Hashtbl.add m key value;
                  continue k ())
          | Get key ->
              Some
                (fun (k : (int, _) continuation) ->
                  continue k (Hashtbl.find m key))
          | _ -> None);
      retc = (fun _ -> ());
      exnc = raise;
    }

let test () =
  handler_v1 @@ interpreter
  @@ [ TrPut ((3, 1), ()); TrGet (3, 1); TrPut ((3, 2), ()); TrGet (3, 2) ]
