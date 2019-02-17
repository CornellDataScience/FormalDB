Require Import List Arith Bool String.
Require String.
Import ListNotations.
Module Table.

Inductive entry :=
    | string_entry (s: string)
    | nat_entry (n : nat).

Definition row : Type :=
    list entry.

Definition header : Type :=
  list string.

Definition table : Type :=
    (list row) * (header).

Check table.
Check row.

Definition empty_table : table 
    := (@nil (row), nil).
Definition add_header (h : header) (tbl: table) :=
  match tbl with
  | (t, old_h) => (t, h)
  end.

Definition add_row (r : row) (tbl : table) :=
    match tbl with 
    | (t, ident) => (r::t, ident)
    end.


Fixpoint filter_row_by_entry (f: entry -> bool) (hdr: string) (h: header) (r: row) : bool :=
  match (h, r) with
  | ([], []) => false
  | (identifier::t, ent::row_tail) => if (string_dec hdr identifier) then (f ent) else (filter_row_by_entry f hdr t row_tail)
  | (_,_) => false (** some sort of error with h and r **)
  end.

Fixpoint filter_table_by_entry_helper (f: entry -> bool) (hdr: string) (t: list row) (h: header) : list row :=
  match t with
  | [] => []
  | r::tl => if (filter_row_by_entry f hdr h r) then (r::(filter_table_by_entry_helper f hdr tl h)) else ((filter_table_by_entry_helper f hdr tl h)) 
  end.                                                     

Fixpoint filter_table_by_entry (f: entry -> bool) (hdr : string) (tbl : table) : table :=
  match tbl with
  | (t, h) => ((filter_table_by_entry_helper f hdr t h), h)
  end.

Theorem 
End Table.