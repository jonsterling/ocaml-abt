module type VARIABLE = sig
  type t
  val fresh : string -> t
  val compare : t * t -> bool
  val to_string : t -> string
  val to_user_string : t -> string
end

module Valence = struct
  type ze = Ze
  type 'n su = Su
end

module Arity = struct
  type nil = Nil
  type ('x, 'xs) cons = Cons
end


module type ENV = sig
  open Arity

  type 'n phi
  type _ t =
    | Nil : nil t
    | Cons : 'n phi * 'ar t -> ('n, 'ar) cons t
end

module EnvNotation (A : ENV) = struct
  open A
  let nil = Nil
  let (@) x xs = Cons (x, xs)
end

module type OPERATOR = sig
  type 'ar t
  val to_string : 'ar t -> string
  val compare : 'a1 t * 'a2 t -> bool
end

module type VIEW = sig
  open Valence

  module Variable : VARIABLE
  module Operator : OPERATOR
  type 'n phi
  type 'n phi' = 'n phi

  module Env : ENV with type 'n phi = 'n phi'

  type _ t =
    | Var : Variable.t -> ze t
    | Abs : Variable.t * 'n phi -> 'n su t
    | App : 'ar Operator.t * 'ar Env.t -> ze t
end


module type ABT = sig
  module Variable : VARIABLE
  module Operator : OPERATOR

  open Valence

  type 'n t
  module Env : ENV with type 'n phi = 'n t

  module View : VIEW
    with type 'n phi = 'n t
    with module Operator = Operator
    with module Variable = Variable
    with module Env = Env

  val into : 'n View.t -> 'n t
  val out : 'n t -> 'n View.t
  val aequiv : 'm t * 'n t -> bool
end

module AbtNotation (A : ABT) = struct
  include A

  open A
  open View

  module EnvNotation = EnvNotation(Env)
  open EnvNotation
  include EnvNotation

  let fresh = Variable.fresh
  let ($) o xs = into (App (o, xs))
  let (^) v e = into (Abs (v, e))
  let var v = into (Var v)
end
