Require Import List Arith Bool String.
Require String.
Import ListNotations.
Module Table.

Inductive entry :=
    | string_entry (s: string)
    | nat_entry (n : nat).

Definition row : Type :=
    list entry.

Definition table : Type :=
    (list row) * (list nat).

Check table.
Check row.

Definition empty_table : table 
    := (@nil (row), nil).

Definition add_row (r : row) (tbl : table) :=
    match tbl with 
    | (t, ident) => (r::t, ident)
    end.

 
End Table.