open Table2
open Csv
open String
open Csv_conv

exception InvalidCSVforTable of string
let csv_lst = Csv.load "Example.csv"

(** Squarify? *)

(* Unit Testing *)

let csv_lst = Csv.square csv_lst
let example_header = CSV_conv.string_list_to_char_matrix (CSV_conv.header_from_csv csv_lst)
let example_entry_rowlist = CSV_conv.entry_rowlist_from_csv_rowlist (CSV_conv.rowlist_from_csv csv_lst)
let dbms = Table2.add_header example_header (Table2.empty_table)

let dbms = CSV_conv.add_rowlist_to_table (example_entry_rowlist) (dbms)

let csv_to_Table2 csv =
  let t_header = CSV_conv.string_list_to_char_matrix (CSV_conv.header_from_csv csv) in
  let t_entry_rowlist = CSV_conv.entry_rowlist_from_csv_rowlist (CSV_conv.rowlist_from_csv csv_lst) in
  let fdb = Table2.add_header t_header (Table2.empty_table) in 
  let fdb = Csv_conv.add_rowlist_to_table (t_entry_rowlist) (fdb)
  in fdb
let () = Table2.print_table (dbms)
let () = print_string "\n-----------------------------\n"
(* let () = Csv.print_readable csv_lst *)
