open Printf
open Sexplib
open Sexplib.Std

type invocation =
  | TrOpen of unit * bool
  | TrClose of unit * unit
  | TrPrepare of unit * bool
  | TrFinalize of unit * bool
  | TrStep of string * bool
[@@deriving sexp]

let to_string = function
  | TrOpen (args, res) ->
      sprintf "%s ← Open %s;"
        (Sexp.to_string ([%sexp_of: bool] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrClose (args, res) ->
      sprintf "%s ← Close %s;"
        (Sexp.to_string ([%sexp_of: unit] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrPrepare (args, res) ->
      sprintf "%s ← Prepare %s;"
        (Sexp.to_string ([%sexp_of: bool] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrFinalize (args, res) ->
      sprintf "%s ← Finalize %s;"
        (Sexp.to_string ([%sexp_of: bool] res))
        (Sexp.to_string ([%sexp_of: unit] args))
  | TrStep (args, res) ->
      sprintf "%s ← Step %s;"
        (Sexp.to_string ([%sexp_of: bool] res))
        (Sexp.to_string ([%sexp_of: string] args))
