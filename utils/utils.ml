module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let run str main =
  let () = Printf.printf "Run %s:\n" str in
  try main () with _ -> Printf.printf "%s Failed.\n" str

open Effect
open Effect.Deep

type _ Effect.t += RandomInt : int t | RandomBool : bool t

let random_handler (main : unit -> 'b) =
  match_with main ()
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | RandomInt ->
              Some (fun (k : (a, _) continuation) -> continue k (Random.int 30))
          | RandomBool ->
              Some
                (fun (k : (bool, _) continuation) ->
                  continue k (Random.bool ()))
          | _ -> None);
      retc = (fun x -> x);
      exnc = raise;
    }
