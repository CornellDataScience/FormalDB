open Table2
open Csv

let csv_lst = Csv.load "Example.csv"
let () = Csv.print_readable csv_lst;
