open Table2
open Csv
open String

module CSV_conv : sig
val print_string_list : (string list) -> unit 

val explode : (string) -> char list

val implode : (char list) -> string

(* CSV functions *)
val header_from_csv : (Csv.t) -> (string list)

val rowlist_from_csv : (Csv.t) -> (string list list)

(** because of Coq, negtaive ints are not ints *)
val is_int : string -> bool

val entry_list_from_csv_row : (string list) -> (Table2.entry list)


val entry_rowlist_from_csv_rowlist : (string list list) -> (Table2.entry list list)


val add_rowlist_to_table : (Table2.entry list list) -> (Table2.table) -> (Table2.table)

val string_list_to_char_matrix : (string list) -> (char list list)

end