open Examples
open Utils

let () = Random.self_init ()
let () = run "Kv_store.gen_c" Kv_store.(main gen_c 6)
let () = run "Kv_store.gen_s" Kv_store.(main gen_s 6)
let () = run "Kv_store.gen_sc" Kv_store.(main gen_sc 6)
let () = run "Locks.gen_c" Locks.(main gen_c 6)
let () = run "Locks.gen_s" Locks.(main gen_s 6)
let () = run "Locks.gen_sc" Locks.(main gen_sc 6)

(* let () = Db_init_lib.test () *)
let sts = [ "SELECT"; "INSERT"; "UPDATE"; "DELETE" ]
let () = run "Db_init.gen_c" Db_init.(main gen_c sts)
let () = run "Db_init.gen_s" Db_init.(main gen_s sts)
let () = run "Db_init.gen_sc" Db_init.(main gen_sc sts)
let () = Maze_lib.test ()
let () = run "Maze.gen_c" Maze.(main gen_s ())
let () = run "Maze.gen_c" Maze.(main gen_c ())
let () = run "Maze.gen_c" Maze.(main gen_sc ())
