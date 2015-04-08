open Signatures
open SimpleVariable
open LocallyNameless
open Valence
open Arity

type _ ops = ..

type ('a, 'b) open_rec = ('a -> 'b) -> 'a -> 'b
  
type ('tm, _, _) judgement =
  | IsType : 'tm -> ('tm, 'tm, unit) judgement

module type JUDGEMENTS = sig
  type tm
  type ('i, 'o) assertion

  val judge : ((tm, 'i, 'o) judgement, 'o) assertion
end

module type OPEN_JUDGEMENTS = sig
  include JUDGEMENTS with type ('i, 'o) assertion = ('i, 'o) open_rec
  exception NoMatch
end

module type CLOSED_JUDGEMENTS = sig
  include JUDGEMENTS with type ('i, 'o) assertion = 'i -> 'o
end

module Compose
    (J1 : OPEN_JUDGEMENTS)
    (J2 : OPEN_JUDGEMENTS with type tm = J1.tm)
  : OPEN_JUDGEMENTS with type tm = J1.tm =
struct
  type tm = J1.tm
  type ('i, 'o) assertion = ('i, 'o) open_rec

  exception NoMatch
  let judge call x =
    try J1.judge call x
    with J1.NoMatch ->
      try J2.judge call x
      with J2.NoMatch -> raise NoMatch
end

module Close (J : OPEN_JUDGEMENTS) : CLOSED_JUDGEMENTS with type tm = J.tm = struct
  type tm = J.tm
  type ('i, 'o) assertion = 'i -> 'o

  let rec judge x = J.judge judge x
end

module type THEORY = sig
  module OperatorSig : sig
    exception NoMatch
    val to_string : 'a ops -> string
    val compare : 'a1 ops * 'a2 ops -> bool
  end

  module TypeChecker
      (Syn : ABT with type 'ar Operator.t = 'ar ops)
    : OPEN_JUDGEMENTS with type tm = ze Syn.t
end

module UnitThy = struct
  type _ ops +=
    | UNIT : nil ops
    | AX : nil ops

  module OperatorSig = struct
    exception NoMatch
    let to_string (type ar) (o : ar ops) =
      match o with
      | UNIT -> "unit"
      | AX -> "ax"
      | _ -> raise NoMatch

    let compare : type a1 a2. a1 ops * a2 ops -> bool = fun x ->
      match x with
      | (UNIT, UNIT) -> true
      | (UNIT, _) -> false
      | (AX, AX) -> true
      | (AX, _) -> false
      | _ -> raise NoMatch
  end

  module TypeChecker (Syn : ABT with type 'ar Operator.t = 'ar ops) = struct
    open Syn
    open Syn.View
    open Syn.View.Env
    
    type tm = ze Syn.t
    type ('i, 'o) assertion = ('i, 'o) open_rec

    exception NoMatch
    exception NotType of tm

    let judge : type i o. ((tm, i, o) judgement, o) assertion = fun call j ->
      match j with
      | IsType tm -> 
        match out tm with
        | App (UNIT, Nil) -> ()
        | App (AX, Nil) -> raise (NotType tm)
        | _ -> raise NoMatch
  end
end


module ProdThy = struct
  type _ ops +=
    | PROD : (ze, (ze, nil) cons) cons ops
    | PAIR : (ze, (ze, nil) cons) cons ops

  module OperatorSig = struct
    exception NoMatch
    let to_string (type ar) (o : ar ops) =
      match o with
      | PROD -> "prod"
      | PAIR -> "pair"
      | _ -> raise NoMatch

    let compare : type a1 a2. a1 ops * a2 ops -> bool = fun x ->
      match x with
      | (PROD, PROD) -> true
      | (PROD, _) -> false
      | (PAIR, PAIR) -> true
      | (PAIR, _) -> false
      | _ -> raise NoMatch
  end
          
  module TypeChecker (Syn : ABT with type 'ar Operator.t = 'ar ops) = struct
    open Syn
    open Syn.View
    open Syn.View.Env
    
    type tm = ze Syn.t
    type ('i, 'o) assertion = ('i, 'o) open_rec

    exception NoMatch
    exception NotType of tm

    let judge : type i o. ((tm, i, o) judgement, o) assertion = fun call j ->
      match j with
      | IsType tm -> 
        match out tm with
        | App (PROD, Cons (a, Cons (b, Nil))) ->
          begin
            call (IsType a);
            call (IsType b)
          end
      | _ -> raise NoMatch
  end
end

module ComposeThy (T1 : THEORY) (T2 : THEORY) : THEORY = struct
  module OperatorSig = struct
    exception NoMatch
    let to_string (type ar) (o : ar ops) =
      try T1.OperatorSig.to_string o
      with T1.OperatorSig.NoMatch ->
        try T2.OperatorSig.to_string o
        with T2.OperatorSig.NoMatch -> raise NoMatch

    let compare : type a1 a2. a1 ops * a2 ops -> bool = fun (o1, o2) ->
      try T1.OperatorSig.compare (o1, o2)
      with T1.OperatorSig.NoMatch ->
        try T2.OperatorSig.compare (o1, o2)
        with T2.OperatorSig.NoMatch -> raise NoMatch
  end

  module TypeChecker (Syn : ABT with type 'ar Operator.t = 'ar ops) = struct
    module TC1 = T1.TypeChecker(Syn)
    module TC2 = T2.TypeChecker(Syn)
    module TC = Compose (TC1) (TC2)
    include TC
  end
end

module EmptyTheory : THEORY = struct
  module OperatorSig = struct
    exception NoMatch
    let to_string (type ar) (o : ar ops) = raise NoMatch
    let compare : type a1 a2. a1 ops * a2 ops -> bool = fun (o1, o2) ->
      raise NoMatch
  end

  module TypeChecker (Syn : ABT with type 'ar Operator.t = 'ar ops) = struct
    type tm = ze Syn.t
    type ('i, 'o) assertion = ('i, 'o) open_rec

    exception NoMatch

    let judge _ _ = raise NoMatch
  end
end

module ComposeThys (TS : sig val theories : (module THEORY) list end) = struct
  let rec thys =
    let rec go = fun (m : (module THEORY)) -> function
      | [] -> m
      | t :: ts ->
        let module T = (val t : THEORY) in
        let module M = (val m : THEORY) in
        let module MT = ComposeThy (M) (T) in
        go (module MT : THEORY) ts
    in go (module EmptyTheory : THEORY) TS.theories
  module Composite = (val thys : THEORY)
  include Composite
end

module Instantiate (T : THEORY) = struct
  module Operator = struct
    type 'a t = 'a ops
    include T.OperatorSig
  end

  module LN = LocallyNameless (SimpleVariable.Variable) (Operator)
  module TypeChecker = T.TypeChecker (LN)
  module Closed = Close(TypeChecker)

  include Closed
end

module Composite = ComposeThys(
  struct
    let theories =
      [ (module UnitThy : THEORY)
      ; (module ProdThy : THEORY)
      ]
  end)

module Test = Instantiate (Composite)
open Test
  
module Notation = AbtNotation (LN)
open Notation

let _ = judge (IsType (ProdThy.PROD $ (UnitThy.UNIT $ nil) @ (UnitThy.UNIT $ nil) @ nil))
