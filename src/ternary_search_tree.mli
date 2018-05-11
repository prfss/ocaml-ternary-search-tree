type ('c, 'v) t
module Pattern_match : sig type 'char t = Literal of 'char | Wildcard end
val create : ('a, 'b) t
val add : ?override:bool -> ('a, 'b) t -> 'a list -> value:'b -> ('a, 'b) t
val remove : ('a, 'b) t -> 'a list -> ('a, 'b) t
val fold : ('a, 'b) t -> init:'c -> f:('c -> 'a list -> 'b -> 'c) -> 'c
val rev_fold : ('a, 'b) t -> init:'c -> f:('c -> 'a list -> 'b -> 'c) -> 'c
val count : ('a, 'b) t -> int
val to_list : ('a, 'b) t -> ('a list * 'b) list
val modify : ('a, 'b) t -> 'a list -> f:('b -> 'b) -> ('a, 'b) t
val search : ('a, 'b) t -> 'a list -> 'b option
val mem : ('a, 'b) t -> 'a list -> bool
val pm_search : ('a, 'b) t -> 'a Pattern_match.t list -> ('a list * 'b) list
val near_search : ('a, 'b) t -> 'a list -> int -> ('a list * 'b) list
val subtree : ('a, 'b) t -> 'a list -> ('a, 'b) t
val set_equal : ('a, 'b) t -> ('a, 'b) t -> bool
