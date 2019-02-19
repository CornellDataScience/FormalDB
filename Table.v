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
Definition entry_eqb (e1: entry) (e2: entry) :=
  match e1, e2 with
  | string_entry s1, string_entry s2 => if (string_dec s1 s2) then true else false
  | nat_entry n1, nat_entry n2 => n1 =? n2
  | _, _ => false
  end.

Fixpoint row_eqb (r1: row) (r2: row) :=
  match r1, r2 with
  | [], [] => true
  | e1::tl1, e2::tl2 => andb (entry_eqb e1 e2) (row_eqb tl1 tl2)
  | _, _ => false
  end.

Fixpoint remove_first_row_from_row_list (r: row) (row_list : list row) :=
  match row_list with
  | [] => []
  | r_elt :: row_list_tl => if (row_eqb r r_elt) then (row_list_tl)
                            else (remove_first_row_from_row_list r
                                                                 row_list_tl)
  end.

Fixpoint remove_all_row_from_row_list (r : row) (row_list : list row) :=
  filter (fun rw => negb (row_eqb rw r)) row_list.

Definition remove_all_row_from_table (r : row) (tbl : table) :=
  match tbl with
  | (row_list, hdr) => ((remove_all_row_from_row_list r row_list), hdr)  
  end.

Definition remove_first_row_from_table (r : row) (tbl : table) :=
  match tbl with
  | (row_list, hdr) => ((remove_first_row_from_row_list r row_list), hdr)
  end.

(** TODO: redifine in terms of fold, filter, or map **)
Fixpoint filter_row_by_entry (f: entry -> bool) (hdr: string) (h: header) (r: row) : bool :=
  match h, r with
  | [], [] => false
  | identifier::t, ent::row_tail => if (string_dec hdr identifier) then (f ent) else (filter_row_by_entry f hdr t row_tail)
  | _, _ => false (** some sort of error with h and r **)
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

(** END unit tests **)
Theorem filter_row_true_implies_length_match : forall (r: row) (h: header) (col_ident : string) (f: entry -> bool),
    filter_row_by_entry f col_ident h r = true -> List.length h = List.length r.
Proof.
  intros r h col_ident f H.
  induction h.
  - simpl in H. destruct r eqn:Heq. discriminate H. discriminate H.
  - simpl in H. induction r.
    + discriminate.
    + 
Abort.
(** these proofs are hardddddddddd :( **)

Theorem remove_first_inverse_of_add_row_on_headed_empty_table : forall (r: row) (h: header) ,
    remove_first_row_from_table r (add_row r (add_header h empty_table)) = (add_header h empty_table).
Proof.
  intros r h.
  simpl. induction r.
  - simpl. reflexivity.
  - simpl. destruct a.
    -- simpl. destruct (string_dec s s). simpl. destruct (row_eqb r r).
    + reflexivity.
    + reflexivity.
    + destruct n. reflexivity.
    -- simpl. induction n.
    + simpl. destruct (row_eqb r r). reflexivity. reflexivity.
    + simpl. apply IHn.
Qed.

(** The false filter is like 0 if filtering is mulitplication *)
Lemma name_this : forall (r : row) (h: header) (col_ident : string) (a: entry) (f: entry -> bool),
    f = (fun e => false) -> filter_row_by_entry f col_ident h (a::r) = filter_row_by_entry f col_ident h r.
Proof.
  intros r h c;ol_ident a f H_f.
  induction r.
  +induction h.
  -simpl. reflexivity.
  -simpl. destruct (string_dec col_ident a0) eqn:H_eq.
   -- rewrite H_f. reflexivity.
   -- induction h.
      --- reflexivity.
      --- reflexivity.
   + induction h.
  - simpl. reflexivity.
  - simpl. destruct (string_dec col_ident a1) eqn: Heq.
    -- rewrite H_f. reflexivity.
    -- 
  +induction h.
  -simpl. reflexivity.
  -simpl. destruct (string_dec col_ident a1) eqn:Heq.
   --rewrite H_f. reflexivity.
   -- 
Abort.
Theorem filter_property_of_false_filter_on_rows : forall (r : row) (h: header) (col_ident : string) (f: entry -> bool),
    f = (fun e => false) -> filter_row_by_entry f col_ident h r = false.
Proof.
  intros r h col_ident f H.
  induction h.
  + induction r.
  -reflexivity.
  -reflexivity.
   + destruct (string_dec col_ident a) eqn:Heq.
  - destruct r. reflexivity. simpl. rewrite Heq. rewrite H. reflexivity.
  - induction r.
    -- reflexivity.
    -- simpl. rewrite Heq. apply IHr.
    induction r.
  - reflexivity.
  - unfold filter_row_by_entry. destruct (string_dec col_ident a) eqn:Heqn.
    -- rewrite H. reflexivity.
    --
Abort.
End Table.