open Printf
open Sexplib
open Sexplib.Std

type invocation =
  | TrUp of unit * unit
  | TrDown of unit * unit
  | TrLeft of unit * unit
  | TrRight of unit * unit
[@@deriving sexp]

let to_string = function
  | TrUp (args, res) ->
      sprintf "%s ← Up %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrDown (args, res) ->
      sprintf "%s ← Down %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrLeft (args, res) ->
      sprintf "%s ← Left %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrRight (args, res) ->
      sprintf "%s ← Right %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: unit] args))
