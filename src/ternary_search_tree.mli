module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Pattern_match : sig type 'char t = Literal of 'char | Wildcard end

module Make :
  functor (Char : Comparable) ->
  sig
    type 'v t
    val create : 'v t
    val add : ?override:bool -> 'v t -> Char.t list -> value:'v -> 'v t
    val remove : 'v t -> Char.t list -> 'v t
    val fold : 'v t -> init:'c -> f:('c -> Char.t list -> 'v -> 'c) -> 'c
    val rev_fold : 'v t -> init:'c -> f:('c -> Char.t list -> 'v -> 'c) -> 'c
    val count : 'v t -> int
    val to_list : 'v t -> (Char.t list * 'v) list
    val modify : 'v t -> Char.t list -> f:('v -> 'v) -> 'v t
    val search : 'v t -> Char.t list -> 'v option
    val mem : 'v t -> Char.t list -> bool
    val pm_search : 'v t -> Char.t Pattern_match.t list -> (Char.t list * 'v) list
    val near_search : 'v t -> Char.t list -> int -> (Char.t list * 'v) list
    val subtree : 'v t -> Char.t list -> 'v t
    val set_equal : 'v t -> 'v t -> bool
  end
