open Sig

(*
module Idea : Sig =
struct
  type 'a _t = ..
  type 'a _t += App of 'a * 'a | Lam of ('a -> 'a)

  (*  type 'a _t = [`App of 'a * 'a | `Lam of ('a -> 'a)] *)

  type 'a t = 'a _t

  type around = around t

  let rec step (t:'a t) (k:'a -> 'a) : 'a =
    match t with
    | App(Lam f, x) -> f x
    | App(t,u) -> App(step t k, step u k)
    | Lam f -> Lam f
    | _ -> k t

  let is_value t k =
    match t with
    | Lam _ -> true
    | App _ -> false
    | _ -> k t


  module View =
  struct
    type t = around

    type view = t

    let _id () = Lam (fun x -> x)

    let view x = x

    let of_view x = x

    let step t = step t (fun _ -> assert false)

    let is_value t = is_value t (fun _ -> assert false)

    let rec mmem x l =
      match l with
      | [] -> false
      | y::l -> if x == y then true else mmem x l

    let mindex x l =
      let rec mindex x l n =
        match l with
        | [] -> raise Not_found
        | y::l -> if x==y then n else mindex x l (n+1)
      in
      mindex x l 0

    let pp fmt t =
      let rec pp env fmt t =
        match t with
        | App(t,u) ->
          Format.fprintf fmt "%a (%a)" (pp env) t (pp env) u
        | Lam f ->
          if mmem t env then Format.fprintf fmt "%d" (mindex t env)
          else
            let id = _id() in
            Format.fprintf fmt ". %a" (pp (id::env)) (f id)
        | _ -> assert false
      in
      pp [] fmt t

    let string_of t =
      Format.asprintf "%a" pp t
  end
end
*)

module Lambda:Sig =
struct
  type 'a t = [`App of 'a * 'a | `Lam of ('a -> 'a)]

  let rec step (t:'a t) (k: 'a t -> 'a) : 'a =
    match t with
    | `App(`Lam f, x) -> f x
    | `App(t,u) -> `App(step t k, step u k)
    | `Lam f -> `Lam f
    | _ -> k t

  let is_value t k =
    match t with
    | `Lam _ -> true
    | `App _ -> false
    | _ -> k t


  module View =
  struct
    type view = view t

    let _id () = `Lam (fun x -> x)

    let view x = x

    let of_view x = x

    let step t = step t (fun _ -> assert false)

    let is_value t = is_value t (fun _ -> assert false)

    let rec mmem x l =
      match l with
      | [] -> false
      | y::l -> if x == y then true else mmem x l

    let mindex x l =
      let rec mindex x l n =
        match l with
        | [] -> raise Not_found
        | y::l -> if x==y then n else mindex x l (n+1)
      in
      mindex x l 0

    let pp fmt t =
      let rec pp env fmt t =
        match t with
        | `App(t,u) ->
          Format.fprintf fmt "%a (%a)" (pp env) t (pp env) u
        | `Lam f ->
          if mmem t env then Format.fprintf fmt "%d" (mindex t env)
          else
            let id = _id() in
            Format.fprintf fmt ". %a" (pp (id::env)) (f id)
      in
      pp [] fmt t

    let string_of t =
      Format.asprintf "%a" pp t
  end
end
(*
module Product(C:Sig) : Sig  =
struct
  type 'a _t = 'a C.t

  type 'a _t += Product of 'a * 'a

  type 'a t = 'a _t

  let step t = failwith "todo"

  let is_value t = failwith "todo"

  module View:View = struct
    type t
    type view

    let view = failwith "todo"

    let of_view = failwith "todo"

    let step t = failwith "todo"

    let is_value = failwith "todo"

    let string_of = failwith "todo"

    let pp fmt t = failwith "todo"
  end
end
*)
