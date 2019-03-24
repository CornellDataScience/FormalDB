open Table
open Csv

let csv_lst = Csv.load "examples/Example.csv"
let () = Csv.print_readable csv_lst ;