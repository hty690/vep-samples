
val negb : bool -> bool

type nat =
| O
| S of nat

type ('a, 'b) sum =
| Coq_inl of 'a
| Coq_inr of 'b

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val coq_CompOpp : comparison -> comparison
