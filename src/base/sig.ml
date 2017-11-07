
module type View = sig

  type t

  type view

  val view : t -> view

  val of_view : view -> t

  val step : view -> view

  val is_value : view -> bool

  val string_of : view -> string

  val pp : Format.formatter -> view -> unit
end

module type Sig = sig
  type 'a t

  val step :  'a t -> ('a t-> 'a) -> 'a

  val is_value :'a t -> ('a t -> bool) -> bool

  module View:View
end
