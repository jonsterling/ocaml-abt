open Signatures
  
module Variable = struct
  type t = {name : string; index : int}

  let counter = ref 0
  let fresh a =
    { name = a
    ; index = (counter := !counter + 1; !counter)
    }

  let compare (x, y) =
    x.index == y.index

  let to_string x = x.name ^ "@" ^ string_of_int x.index
  let to_user_string x = x.name
end
