open Maze_sig
open Effect
open Effect.Deep
open Printf

type _ Effect.t +=
  | Up : unit -> unit t
  | Down : unit -> unit t
  | Left : unit -> unit t
  | Right : unit -> unit t

let interpreter (tr : invocation list) =
  let step ivc =
    match ivc with
    | TrUp (args, res) ->
        let res' = perform (Up args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrDown (args, res) ->
        let res' = perform (Down args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrLeft (args, res) ->
        let res' = perform (Left args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
    | TrRight (args, res) ->
        let res' = perform (Right args) in
        if res == res' then printf "%s\n" @@ to_string ivc
        else failwith "cannot pass the check"
  in
  fun () -> List.iter step tr

let bound = (4, 5)

let is_valid p bound =
  let a, b = p in
  (* let _ = Printf.printf "p: (%i, %i)\n" a b in *)
  let a', b' = bound in
  a >= 0 && a < a' && b >= 0 && b < b'

let go_up (x, y) = (x + 1, y)
let go_down (x, y) = (x - 1, y)
let go_left (x, y) = (x, y - 1)
let go_right (x, y) = (x, y + 1)

let handler (main : unit -> unit) =
  let p = ref (0, 0) in
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Up () ->
              Some
                (fun (k : (a, _) continuation) ->
                  let p' = go_up !p in
                  if is_valid p' bound then (
                    p := p';
                    continue k ())
                  else failwith "bad action")
          | Down () ->
              Some
                (fun (k : (a, _) continuation) ->
                  let p' = go_down !p in
                  if is_valid p' bound then (
                    p := p';
                    continue k ())
                  else failwith "bad action")
          | Left () ->
              Some
                (fun (k : (a, _) continuation) ->
                  let p' = go_left !p in
                  if is_valid p' bound then (
                    p := p';
                    continue k ())
                  else failwith "bad action")
          | Right () ->
              Some
                (fun (k : (a, _) continuation) ->
                  let p' = go_right !p in
                  if is_valid p' bound then (
                    p := p';
                    continue k ())
                  else failwith "bad action")
          | _ -> None);
      retc = (fun _ -> ());
      exnc = raise;
    }

let test () =
  handler @@ interpreter
  @@ [ TrUp ((), ()); TrRight ((), ()); TrUp ((), ()); TrRight ((), ()) ]
