module type VARIABLE =
  sig
    type t
    val fresh : string -> t
    val compare : t * t -> bool
    val to_string : t -> string
    val to_user_string : t -> string
  end
module Valence : sig type ze = Ze type 'n su = Su end
module Arity : sig type nil = Nil type ('x, 'xs) cons = Cons end
module type ENV =
  sig
    type 'n phi
    type _ t =
        Nil : Arity.nil t
      | Cons : 'n phi * 'ar t -> ('n, 'ar) Arity.cons t
  end
module EnvNotation :
  functor (A : ENV) ->
    sig
      val nil : Arity.nil A.t
      val ( @ ) : 'a A.phi -> 'b A.t -> ('a, 'b) Arity.cons A.t
    end
module type OPERATOR =
  sig
    type 'ar t
    val to_string : 'ar t -> string
    val compare : 'a1 t * 'a2 t -> bool
  end
module type VIEW =
  sig
    module Variable : VARIABLE
    module Operator : OPERATOR
    type 'n phi
    type 'n phi' = 'n phi
    module Env :
      sig
        type 'n phi = 'n phi'
        type _ t =
            Nil : Arity.nil t
          | Cons : 'n phi * 'ar t -> ('n, 'ar) Arity.cons t
      end
    type _ t =
        Var : Variable.t -> Valence.ze t
      | Abs : Variable.t * 'n phi -> 'n Valence.su t
      | App : 'ar Operator.t * 'ar Env.t -> Valence.ze t
  end
module type ABT =
  sig
    module Variable : VARIABLE
    module Operator : OPERATOR
    type 'n t
    module Env :
      sig
        type 'n phi = 'n t
        type _ t =
            Nil : Arity.nil t
          | Cons : 'n phi * 'ar t -> ('n, 'ar) Arity.cons t
      end
    module View :
      sig
        module Variable :
          sig
            type t = Variable.t
            val fresh : string -> t
            val compare : t * t -> bool
            val to_string : t -> string
            val to_user_string : t -> string
          end
        module Operator :
          sig
            type 'ar t = 'ar Operator.t
            val to_string : 'ar t -> string
            val compare : 'a1 t * 'a2 t -> bool
          end
        type 'n phi = 'n t
        type 'n phi' = 'n phi
        module Env :
          sig
            type 'n phi = 'n t
            type 'a t =
              'a Env.t =
                Nil : Arity.nil t
              | Cons : 'n phi * 'ar t -> ('n, 'ar) Arity.cons t
          end
        type _ t =
            Var : Variable.t -> Valence.ze t
          | Abs : Variable.t * 'n phi -> 'n Valence.su t
          | App : 'ar Operator.t * 'ar Env.t -> Valence.ze t
      end
    val into : 'n View.t -> 'n t
    val out : 'n t -> 'n View.t
    val aequiv : 'm t * 'n t -> bool
  end
module AbtNotation :
  functor (A : ABT) ->
    sig
      module Variable :
        sig
          type t = A.Variable.t
          val fresh : string -> t
          val compare : t * t -> bool
          val to_string : t -> string
          val to_user_string : t -> string
        end
      module Operator :
        sig
          type 'ar t = 'ar A.Operator.t
          val to_string : 'ar t -> string
          val compare : 'a1 t * 'a2 t -> bool
        end
      type 'n t = 'n A.t
      module Env :
        sig
          type 'n phi = 'n t
          type 'a t =
            'a A.Env.t =
              Nil : Arity.nil t
            | Cons : 'n phi * 'ar t -> ('n, 'ar) Arity.cons t
        end
      module View :
        sig
          module Variable :
            sig
              type t = Variable.t
              val fresh : string -> t
              val compare : t * t -> bool
              val to_string : t -> string
              val to_user_string : t -> string
            end
          module Operator :
            sig
              type 'ar t = 'ar Operator.t
              val to_string : 'ar t -> string
              val compare : 'a1 t * 'a2 t -> bool
            end
          type 'n phi = 'n t
          type 'n phi' = 'n phi
          module Env :
            sig
              type 'n phi = 'n t
              type 'a t =
                'a Env.t =
                  Nil : Arity.nil t
                | Cons : 'n phi * 'ar t -> ('n, 'ar) Arity.cons t
            end
          type 'a t =
            'a A.View.t =
              Var : Variable.t -> Valence.ze t
            | Abs : Variable.t * 'n phi -> 'n Valence.su t
            | App : 'ar Operator.t * 'ar Env.t -> Valence.ze t
        end
      val into : 'n View.t -> 'n t
      val out : 'n t -> 'n View.t
      val aequiv : 'm t * 'n t -> bool
      module EnvNotation :
        sig
          val nil : Arity.nil A.View.Env.t
          val ( @ ) :
            'a A.View.Env.phi ->
            'b A.View.Env.t -> ('a, 'b) Arity.cons A.View.Env.t
        end
      val nil : Arity.nil A.View.Env.t
      val ( @ ) :
        'a A.View.Env.phi ->
        'b A.View.Env.t -> ('a, 'b) Arity.cons A.View.Env.t
      val fresh : string -> A.View.Variable.t
      val ( $ ) : 'a A.View.Operator.t -> 'a A.View.Env.t -> Valence.ze A.t
      val ( ^ ) : A.View.Variable.t -> 'a A.View.phi -> 'a Valence.su A.t
      val var : A.View.Variable.t -> Valence.ze A.t
    end
