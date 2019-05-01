open Table2
open Csv
open String
open Csv_conv

exception NoFileFound of string
let csv_filename = try (Array.get Sys.argv 1) with e -> (print_string "ERROR while attempting to read filename \n"; exit 0; "")
let csv_lst = try (Csv.load csv_filename) with e -> (print_string ("ERROR trying to open file " ^ csv_filename ^ "\n"); raise (NoFileFound csv_filename))

let csv_lst = Csv.square csv_lst

let rec print_row r =
  match r with 
  | [] -> ()
  | h::t -> match h with 
            | Table2.Coq_nat_entry n -> print_float n; print_string "\t"; print_row t
            | Table2.Coq_nil_entry -> print_string ("[NULL_ENTRY]" ^ "\t"); print_row t
            | Table2.Coq_string_entry s -> print_string (CSV_conv.implode(s) ^ "\t"); print_row t

let csv_to_Table2 csv =
  let t_header = CSV_conv.string_list_to_char_matrix (CSV_conv.header_from_csv csv) in
<<<<<<< HEAD
  let t_entry_rowlist = CSV_conv.entry_rowlist_from_csv_rowlist (CSV_conv.rowlist_from_csv csv) in
  let fdb = Table2.add_header t_header (Table2.empty_table) in 
=======
  let t_entry_rowlist = CSV_conv.entry_rowlist_from_csv_rowlist (CSV_conv.rowlist_from_csv csv_lst) in
  let fdb = Table2.add_header t_header (Table2.empty_table) in
>>>>>>> fc2872165ad376b8de0d362a234f77ffe2099416
  let fdb = CSV_conv.add_rowlist_to_table (t_entry_rowlist) (fdb)
  in fdb

let select_where (h_iden : string) (target : string) (tbl: Table2.table) =
  let h_ident_char_list = CSV_conv.explode (h_iden) in
  let filter_function (e : Table2.entry) =
    match e with
    | Table2.Coq_string_entry s -> String.equal (CSV_conv.implode s) target
    | Table2.Coq_nat_entry f -> begin
                                match (float_of_string_opt target) with 
                                | None -> false
                                | Some target_float -> f = target_float
                                end
                                  
    | _ -> false
  in
  Table2.filter_table_by_entry filter_function h_ident_char_list tbl

let rec trim_comma_whitespace str =
  match (str) with
  | [] -> []
  | ','::' '::rest -> trim_comma_whitespace (','::rest)
  | h::t -> h::trim_comma_whitespace(t)

let add_row (row_string : string) (tbl : Table2.table) = 
  let csv_list = String.split_on_char ',' (row_string |> CSV_conv.explode |> trim_comma_whitespace |> CSV_conv.implode) in 
  let e_list = CSV_conv.entry_list_from_csv_row csv_list in 
  Table2.add_row e_list tbl 

let add_table (table_alias : string) (table_path) (tbl_list) : unit =
  let added_csv_lst = try (Csv.load table_path) 
  with e -> (print_string ("ERROR trying to open file " ^ table_path ^ "\n"); 
  raise (NoFileFound table_path)) in
  let added_table_2 = (csv_to_Table2 added_csv_lst) in 
  Hashtbl.add tbl_list table_alias added_table_2


type command =
 | Exit
 | Select_where of (string * string)
<<<<<<< HEAD
 | Select_from_where_eq of (string * string * string)
 | Print of string 
=======
 | Print
>>>>>>> fc2872165ad376b8de0d362a234f77ffe2099416
 | Malformed
 | Add_row of string
 | Load of (string * string)

let parse (cmd) =
 match ((String.split_on_char ' ' cmd): string list) with
 | [] -> Malformed
<<<<<<< HEAD
 | h::[] -> begin 
            match h with 
            | "print" -> Print "main"
=======
 | h::[] -> begin
            match h with
            | "print" -> Print
>>>>>>> fc2872165ad376b8de0d362a234f77ffe2099416
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
           | "select" when ((List.hd t) = "from") ->begin
                                                    match t with 
                                                    | from::alias::whr::h_iden::eq::target::[] when (whr = "where" && eq = "=") -> 
                                                      Select_from_where_eq (alias, h_iden, target)
                                                    | _ -> Malformed
                                                    end                          
           | "add" when ((List.hd t) = "row") -> begin 
                                                 match t with 
                                                 | rw::rw_lst -> begin 
                                                                 try Add_row (String.sub cmd 8 ((String.length cmd) - 8))
                                                                 with e -> Malformed
                                                                 end
                                                 |_ -> Malformed
                                                 end    
            | "load" -> begin 
                        match t with 
                        | filename::_as::alias::[] when _as = "as" -> 
                                        begin 
                                        Load (alias, filename)
                                        end
                        |_ -> Malformed
                        end 
           | "print" -> begin 
                        match t with 
                        | alias::[] -> Print alias 
                        | _ -> Malformed    
                        end                                  
           | _ -> Malformed
           end


let rec main_loop (tbl : Table2.table) (tbl_list : ((string, Table2.table) Hashtbl.t))=
  match read_line () |> parse with
  | Exit -> exit 0;
<<<<<<< HEAD
  | Print alias ->  begin 
                    match alias with 
                    | "main" -> Table2.print_table (tbl); main_loop tbl tbl_list
                    | other_alias -> Table2.print_table (Hashtbl.find tbl_list alias); main_loop tbl tbl_list
                    end
  | Malformed -> print_string ("Error: Malformed command\n"); main_loop tbl tbl_list
  | Select_where (h_iden, target) -> begin 
                                     print_string ("Here is the resultant table: \n\n"); 
                                     let new_tbl = select_where h_iden target tbl in
                                     Table2.print_table (new_tbl); main_loop (new_tbl) tbl_list
                                     end
  | Select_from_where_eq (alias, h_iden, target) -> begin
                                                    match alias with 
                                                    | "main" -> 
                                                      begin 
                                                      print_string ("Here is the resultant table: \n\n"); 
                                                      let new_tbl = select_where h_iden target tbl in
                                                      Table2.print_table (new_tbl); main_loop (new_tbl) tbl_list
                                                      end
                                                    | other_alias -> 
                                                      begin
                                                      print_string ("Here is the resultant table: \n\n"); 
                                                      let new_tbl = select_where h_iden target (Hashtbl.find tbl_list alias) in
                                                      let () = Hashtbl.add tbl_list alias new_tbl in
                                                      Table2.print_table (new_tbl); main_loop (tbl) tbl_list
                                                      end
                                                    end
  | Add_row s -> print_string ("Here is the resultant table: \n\n");
                 let new_tbl = add_row s tbl in 
                 Table2.print_table (new_tbl); main_loop (new_tbl) tbl_list
  | Load (alias, filename) -> print_string ("Loading in table " ^ alias ^ "\n");
                              let () = add_table alias filename tbl_list in 
                              print_string ("Added table " ^ alias ^ "\n\n"); 
                              Table2.print_table (Hashtbl.find tbl_list alias); 
                              main_loop tbl tbl_list
            

let fdb = csv_to_Table2 csv_lst
let () = print_string "Here is the prettified CSV output loaded into the FormalDB \n\n"
let () = Table2.print_table (fdb) 
let () = main_loop fdb (Hashtbl.create 5)
=======
  | Print -> Table2.print_table (tbl); main_loop tbl
  | Malformed -> print_string ("Error: Malformed command\n"); main_loop tbl
  | Select_where (h_iden, target) -> print_string ("Here is the resultant table: \n\n");
                                     let new_tbl = select_where h_iden target tbl in
                                     Table2.print_table (new_tbl); main_loop (new_tbl)


let fdb = csv_to_Table2 csv_lst
let () = print_string "Here is the prettified CSV output loaded into the FormalDB \n\n"
let () = Table2.print_table (fdb)
let () = main_loop fdb
>>>>>>> fc2872165ad376b8de0d362a234f77ffe2099416
