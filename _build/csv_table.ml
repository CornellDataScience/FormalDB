open Table2
open Csv
open String

exception InvalidCSVforTable of string
let csv_lst = Csv.load "Example.csv"

(* Utility Functions *)
let rec print_string_list lst = 
    match lst with
    | [] -> ()
    | h::t -> print_string (h ^ "\t"); print_string_list t

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

(* CSV functions *)
let header_from_csv csv =
    match csv with 
    | [] -> raise (InvalidCSVforTable "empty CSV")
    | h::t -> h

let rowlist_from_csv csv =
    match csv with 
    | [] -> raise (InvalidCSVforTable "empty CSV")
    | h::t -> t

(** because of Coq, negtaive ints are not ints *)
let is_int s =
    try (int_of_string s); ((int_of_string s) >= 0)
    with _ -> false

let rec entry_list_from_csv_row row =
    match row with 
    | [] -> []
    | h::t -> (Table2.Coq_string_entry (explode h)) :: (entry_list_from_csv_row t)


let rec entry_rowlist_from_csv_rowlist rowlist = 
    match rowlist with 
    | [] -> []
    | h::t -> (entry_list_from_csv_row h) :: (entry_rowlist_from_csv_rowlist t)

let add_rowlist_to_table (rowlist) (tbl : Table2.table) =
    let rec helper (rowlist) (tbl) = 
    match rowlist with
    | [] -> tbl
    | h::t -> helper (t) (Table2.add_row h tbl)
    in helper (List.rev rowlist) tbl

let rec string_list_to_char_matrix (lst) =
    match lst with 
    | [] -> []
    | h :: t -> (explode h) :: (string_list_to_char_matrix t)


(** Squarify? *)

let example_header = string_list_to_char_matrix (header_from_csv csv_lst)
let example_entry_rowlist = entry_rowlist_from_csv_rowlist (rowlist_from_csv csv_lst)
let dbms = Table2.add_header example_header (Table2.empty_table)

let dbms = add_rowlist_to_table (example_entry_rowlist) (dbms)

let () = Table2.print_table (dbms)
let () = print_string "\n-----------------------------\n"
(* let () = Csv.print_readable csv_lst *)
