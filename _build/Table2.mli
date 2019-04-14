
type __ = Obj.t

val negb : bool -> bool

val length : 'a1 list -> int

module Nat :
 sig
  val eqb : int -> int -> bool
 end

val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list

val string_dec : char list -> char list -> bool

module Table2 :
 sig
  val string_eq : char list -> char list -> bool

  type entry =
  | Coq_string_entry of char list
  | Coq_nat_entry of int
  | Coq_nil_entry

  val entry_rect : (char list -> 'a1) -> (int -> 'a1) -> 'a1 -> entry -> 'a1

  val entry_rec : (char list -> 'a1) -> (int -> 'a1) -> 'a1 -> entry -> 'a1

  type row = entry list

  type header = char list list

  type table = row list * header

  val get_rowlist : table -> row list

  val get_header : table -> header

  val empty_table : table

  val add_header : header -> table -> row list * header

  val entry_type_match : entry -> entry -> bool

  val row_validity_2_3_bool : row -> row -> bool

  val row_validity_2_3_table : row -> row list -> bool

  val header_matches_first_row : header -> row -> row list -> bool

  val add_row : row -> table -> table

  val entry_eqb : entry -> entry -> bool

  val row_eqb : row -> row -> bool

  val remove_first_row_from_row_list : row -> row list -> row list

  val remove_all_row_from_row_list : row -> row list -> row list

  val remove_all_row_from_table : row -> table -> row list * header

  val remove_first_row_from_table : row -> table -> row list * header

  val filter_row_by_entry : (entry -> bool) -> char list -> header -> row -> bool

  val filter_table_by_entry_helper :
    (entry -> bool) -> char list -> row list -> header -> row list

  val filter_table_by_entry : (entry -> bool) -> char list -> table -> table

  val table_valid_rect : table -> (__ -> __ -> __ -> 'a1) -> 'a1

  val table_valid_rec : table -> (__ -> __ -> __ -> 'a1) -> 'a1

  val entry_is_haram : entry -> bool

  val entry_is_string : char list -> entry -> bool

  val print_table : table -> unit
 end
