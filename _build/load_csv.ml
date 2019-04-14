open Table2
open Csv
open String
open Csv_conv

exception NoFileFound of string
let csv_filename = try (Array.get Sys.argv 1) with e -> (print_string "ERROR while attempting to read filename \n"; exit 0; "")
let csv_lst = try (Csv.load csv_filename) with e -> (print_string ("ERROR trying to open file " ^ csv_filename ^ "\n"); raise (NoFileFound csv_filename))

let csv_lst = Csv.square csv_lst

let csv_to_Table2 csv =
  let t_header = CSV_conv.string_list_to_char_matrix (CSV_conv.header_from_csv csv) in
  let t_entry_rowlist = CSV_conv.entry_rowlist_from_csv_rowlist (CSV_conv.rowlist_from_csv csv_lst) in
  let fdb = Table2.add_header t_header (Table2.empty_table) in 
  let fdb = CSV_conv.add_rowlist_to_table (t_entry_rowlist) (fdb)
  in fdb

let fdb = csv_to_Table2 csv_lst
let () = print_string "Here is the prettified CSV output loaded into the FormalDB \n\n"
let () = Table2.print_table (fdb)