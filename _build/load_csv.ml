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

let select_where (h_iden : string) (target : string) (tbl: Table2.table) =
  let h_ident_char_list = CSV_conv.explode (h_iden) in
  let filter_function (e : Table2.entry) = 
    match e with 
    | Table2.Coq_string_entry s -> String.equal (CSV_conv.implode s) target
    | _ -> false
  in 
  Table2.filter_table_by_entry filter_function h_ident_char_list tbl

type command =
 | Exit 
 | Select_where of (string * string)
 | Print 
 | Malformed

let parse (cmd) = 
 match ((String.split_on_char ' ' cmd): string list) with 
 | [] -> Malformed
 | h::[] -> begin 
            match h with 
            | "print" -> Print
            | "exit" -> Exit
            | _ -> Malformed  
            end
 | h::t -> begin
           match h with 
           | "select" when ((List.hd t) = "where") -> begin
                                                    match t with 
                                                    | whr::h_iden::eq::target::[] -> Select_where (h_iden,target)
                                                    | _ -> Malformed
                                                    end
           | _ -> Malformed
           end
      



let rec main_loop (tbl : Table2.table) =
  match read_line () |> parse with
  | Exit -> exit 0;
  | Print -> Table2.print_table (tbl); main_loop tbl
  | Malformed -> print_string ("Error: Malformed command\n"); main_loop tbl
  | Select_where (h_iden, target) -> print_string ("Here is the resultant table: \n\n"); 
                                     let new_tbl = select_where h_iden target tbl in
                                     Table2.print_table (new_tbl); main_loop (new_tbl)
            

let fdb = csv_to_Table2 csv_lst
let () = print_string "Here is the prettified CSV output loaded into the FormalDB \n\n"
let () = Table2.print_table (fdb) 
let () = main_loop fdb