open Signatures

module LocallyNameless (V : VARIABLE) (O : OPERATOR) = struct
  module Variable = V
  module Operator = O

  open Valence
  open Arity

  module Environment (M : sig type 'n phi end) : ENV with type 'n phi = 'n M.phi = struct
    type 'n phi = 'n M.phi
    type _ t =
      | Nil : nil t
      | Cons : 'n phi * 'ar t -> ('n, 'ar) cons t
  end

  type _ t =
    | Free : V.t -> ze t
    | Bound : int -> ze t
    | Abs : 'n t -> 'n su t
    | App : 'ar O.t * 'ar env -> ze t
  and _ env =
    | Nil : nil env
    | Cons : 'n t * 'ar env -> ('n, 'ar) cons env

  module Env = Environment(struct type 'n phi = 'n t end)

  type 'n tm = 'n t
  module View = struct
    module Env = Env
    module Operator = O
    module Variable = V

    type 'n phi = 'n t
    type 'n phi' = 'n phi

    type _ t =
      | Var : V.t -> ze t
      | Abs : V.t * 'n tm -> 'n su t
      | App : 'ar O.t * 'ar Env.t -> ze t
  end

  exception Hole

  let rec aequiv : type m n. m tm * n tm -> bool = function
    | (Bound m, Bound n) -> m == n
    | (Abs e1, Abs e2) -> aequiv (e1, e2)
    | (App (o, rho), App (o', rho')) -> O.compare (o, o') && env_compare (rho, rho')
    | _ -> false
  and env_compare : type ar1 ar2. ar1 env * ar2 env -> bool = function
    | (Nil, Nil) -> true
    | (Cons (x,xs), Cons (y, ys)) -> aequiv (x, y) && env_compare (xs, ys)
    | _ -> false


  let rec shift_var : type n. V.t * int * n tm -> n tm =
    fun (v, n, tm) ->
      match tm with
      | Free v' -> if V.compare (v, v') then Bound n else Free v'
      | Bound m -> Bound m
      | Abs e -> Abs (shift_var (v, (n + 1), e))
      | App (o, rho) -> App (o, shift_var_env (v, n, rho))
  and shift_var_env : type ar. V.t * int * ar env -> ar env =
    fun (v, n, rho) ->
      match rho with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (shift_var (v, n, x), shift_var_env (v, n, xs))

  (* TODO: find a way to remove this kludge *)
  let rec env2Env : type ar. ar Env.t -> ar env = fun xs ->
    match xs with
    | Env.Nil -> Nil
    | Env.Cons (y, ys) -> Cons (y, env2Env ys)
  let rec env2Env' : type ar. ar env -> ar Env.t = fun xs ->
    match xs with
    | Nil -> Env.Nil
    | Cons (y, ys) -> Env.Cons (y, env2Env' ys)
      
  let rec into : type n. n View.t -> n t = fun x ->
    match x with
    | View.Var v -> Free v
    | View.Abs (v, e) -> Abs (shift_var (v, 0, e))
    | View.App (o, rho) -> App (o, env2Env rho)

  let rec add_var : type n. V.t * int * n tm -> n tm =
    fun (v, n, tm) ->
      match tm with
      | Free v' -> Free v'
      | Bound m -> if m == n then Free v else Bound m
      | Abs e -> Abs (add_var (v, n + 1, e))
      | App (o, rho) -> App (o, add_var_env (v, n, rho))
  and add_var_env : type ar. V.t * int * ar env -> ar env =
    fun (v, n, rho) ->
      match rho with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (add_var (v, n, x), add_var_env (v, n, xs))
      
  exception MisplacedBoundVariable
  let out : type n. n t -> n View.t = fun x ->
    match x with
    | Free v -> View.Var v
    | Bound _ -> raise MisplacedBoundVariable
    | Abs e -> let v = V.fresh "x" in View.Abs (v, add_var (v, 0, e))
    | App (o, rho) -> View.App (o, env2Env' rho)
end
