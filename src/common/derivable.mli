open Batteries

module type OrderedType = sig
  type t
  [@@deriving yojson]

  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end

module Int : OrderedType with type t = int

module type SetS = sig
  include Set.S

  val pp_gen : open_:string -> close:string -> empty:string -> Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
end

module type MapS = sig
  include Map.S

  val map_filter : ('a -> 'b option) -> 'a t -> 'b t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (* FIXME: for < 4.06 compatibility *)

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  val of_yojson : (Yojson.Safe.t -> 'a Ppx_deriving_yojson_runtime.error_or) -> Yojson.Safe.t -> 'a t Ppx_deriving_yojson_runtime.error_or
end

module MakeSet (Ord : OrderedType) : SetS with type elt = Ord.t
module MakeMap (Ord : OrderedType) : MapS with type key = Ord.t
