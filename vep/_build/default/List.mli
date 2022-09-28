open Datatypes

val concat : 'a1 list list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list
