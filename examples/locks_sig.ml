open Printf
open Sexplib
open Sexplib.Std

type invocation =
  | TrNewLock of int * unit
  | TrLock of int * unit
  | TrUnlock of int * unit
[@@deriving sexp]

let to_string = function
  | TrNewLock (args, res) ->
      sprintf "%s ← NewLock %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: int] args))
  | TrLock (args, res) ->
      sprintf "%s ← Lock %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: int] args))
  | TrUnlock (args, res) ->
      sprintf "%s ← Unlock %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: int] args))
