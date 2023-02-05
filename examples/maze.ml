open Maze_sig
open Effect
open Effect.Deep
open Utils

type _ Effect.t +=
  | MkUp : (unit * unit) -> unit t
  | MkDown : (unit * unit) -> unit t
  | MkLeft : (unit * unit) -> unit t
  | MkRight : (unit * unit) -> unit t
  | GetPrev : (int * int) list t

let gen_s () =
  perform (MkUp ((), ()));
  perform (MkUp ((), ()))

let gen_c () =
  let len = perform RandomInt in
  let step () =
    let i = perform RandomInt mod 4 in
    match i with
    | 0 -> perform (MkUp ((), ()))
    | 1 -> perform (MkDown ((), ()))
    | 2 -> perform (MkLeft ((), ()))
    | _ -> perform (MkRight ((), ()))
  in
  let _ = List.init len (fun _ -> step ()) in
  ()

let peq (a, b) (a', b') = a == a' && b == b'

let rec gen_sc () =
  let prev = perform GetPrev in
  let x, y = List.nth prev (List.length prev - 1) in
  let actions =
    [
      ((x + 1, y), MkUp ((), ()));
      ((x - 1, y), MkDown ((), ()));
      ((x, y + 1), MkRight ((), ()));
      ((x, y - 1), MkLeft ((), ()));
    ]
  in
  let actions =
    List.filter_map
      (fun (p', action) ->
        if Maze_lib.(is_valid p' bound) && not (List.exists (peq p') prev) then
          Some action
        else None)
      actions
  in
  if List.length actions == 0 then ()
  else
    let i = perform RandomInt mod (List.length actions + 1) in
    if i == List.length actions then ()
    else
      let action = List.nth actions i in
      perform action;
      gen_sc ()

let handler (main : unit -> unit) =
  let tr = ref [] in
  let prev = ref [ (0, 0) ] in
  let append e =
    let x, y = List.nth !prev (List.length !prev - 1) in
    match e with
    | TrUp _ -> prev := !prev @ [ (x + 1, y) ]
    | TrDown _ -> prev := !prev @ [ (x - 1, y) ]
    | TrLeft _ -> prev := !prev @ [ (x, y - 1) ]
    | TrRight _ -> prev := !prev @ [ (x, y + 1) ]
  in
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | MkUp (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let e = TrUp (args, res) in
                  append e;
                  tr := !tr @ [ e ];
                  continue k ())
          | MkDown (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let e = TrDown (args, res) in
                  append e;
                  tr := !tr @ [ e ];
                  continue k ())
          | MkLeft (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let e = TrLeft (args, res) in
                  append e;
                  tr := !tr @ [ e ];
                  continue k ())
          | MkRight (args, res) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let e = TrRight (args, res) in
                  append e;
                  tr := !tr @ [ e ];
                  continue k ())
          | GetPrev -> Some (fun (k : (a, _) continuation) -> continue k !prev)
          | _ -> None);
      retc =
        (fun _ ->
          (* let () = *)
          (*   List.iter (fun (a, b) -> Printf.printf "(%i, %i) -->" a b) !prev *)
          (* in *)
          (* let () = Printf.printf "\n" in *)
          !tr);
      exnc = raise;
    }

let tr_gen gen n = random_handler (fun () -> handler (fun _ -> gen n))
let main gen n () = Maze_lib.handler @@ Maze_lib.interpreter @@ tr_gen gen n
