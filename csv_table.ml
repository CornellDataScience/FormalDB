open Table2
open Csv

let csv_lst = Csv.load "Example.csv"
let rec print_string_list lst = 
    match lst with
    | [] -> ()
    | h::t -> print_string (h ^ "\n"); print_string_list t

let () = print_string_list (List.hd csv_lst) 
let () = Csv.print_readable csv_lst
