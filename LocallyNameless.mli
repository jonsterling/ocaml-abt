module LocallyNameless (V : Signatures.VARIABLE) (O : Signatures.OPERATOR) : sig
  include Signatures.ABT
    with module Variable = V
    with module Operator = O
  exception MisplacedBoundVariable
end
