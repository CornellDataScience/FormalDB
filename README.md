# FormalDB
We are setting out to build a prototype formally verified database system. Formal verification is a technique of mathematically proving certain functionality and algorithms. Essentially pre and post conditions are represented as mathematical theorems and proven with a mechanical proof solving system. The core system would be built in Coq and compiled down to OCaml. I/O and command parsing will be written in OCaml.

## load_csv.ml is the main OCaml layer app, with dependencies Table2.ml, and Csv_conv.ml

## Table.v is the main Coq layer app

## frontend/frontend.js is the main Node layer app for demo deployment 
