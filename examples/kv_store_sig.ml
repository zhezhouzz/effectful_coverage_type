open Printf
open Sexplib
open Sexplib.Std

type invocation = TrPut of (int * int) * unit | TrGet of int * int
[@@deriving sexp]

let to_string = function
  | TrPut (args, res) ->
      sprintf "%s ← Put %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: int * int] args))
  | TrGet (args, res) ->
      sprintf "%s ← Get %s;"
        (Sexp.to_string ([%sexp_of: int] res))
        (Sexp.to_string ([%sexp_of: int] args))
