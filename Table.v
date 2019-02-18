Require Import List Arith Bool Ascii String.
Require String.
Import ListNotations.
Module Table.
Open Scope string_scope.

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

(** Unit tests with definitions so far **)
Let test_tbl :=
  add_header ([ "Name" ; "Year" ;"Address" ; "Major" ]) empty_table.


Let test_tbl_1 :=
  add_row [(string_entry "Ahad"); (nat_entry 2021); (string_entry "Address"); (string_entry "Computer Science")] test_tbl.

Let entry_is_haram (e : entry) :=
  match e with
  | string_entry s => if (string_dec s "Haram") then true else false 
  | _ => false
  end.

Let entry_is_string (s : string) (e : entry) :=
  match e with
  | string_entry s_e => if (string_dec s_e s) then true else false
  | _ => false
  end.

Theorem tbl_unit_test_1 :
  filter_table_by_entry (entry_is_haram) ("Name") (test_tbl_1) = test_tbl.
Proof. simpl. unfold test_tbl. simpl. reflexivity. Qed.
Theorem tbl_unit_test_2 :
  filter_table_by_entry (entry_is_string "Ahad") ("Name") (test_tbl_1) = test_tbl_1.
Proof. reflexivity. Qed.

End Table.