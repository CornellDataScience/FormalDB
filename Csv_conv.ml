open Table2
open Csv
open String

exception InvalidCSVforTable of string


module CSV_conv = struct 
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
let is_float s =
    try (float_of_string s); true 
    with _ -> false
let rec entry_list_from_csv_row row =
    match row with 
    | [] -> []
    | h::t when (h = "") -> Table2.Coq_nil_entry :: (entry_list_from_csv_row t)
    | h::t when (is_float h) -> Table2.Coq_nat_entry (float_of_string h) :: (entry_list_from_csv_row t)
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

end 