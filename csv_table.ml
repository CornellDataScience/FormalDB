open Table
open Csv

let csv_lst = Csv.load "~Desktop/formaldb/FormalDB/Example.csv"
let () = Csv.print_readable csv_lst ;