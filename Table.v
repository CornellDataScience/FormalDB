Require Import List Arith Bool Ascii String.
Require String.
Import ListNotations.
From Coq Require Extraction.
Module Table.
Open Scope string_scope.

Definition string_eq s1 s2 : bool:=
  if (string_dec s1 s2) then true else false.

Inductive entry :=
    | string_entry (s: string)
    | nat_entry (n : nat)
    | nil_entry.


Definition row : Type :=
    list entry.

Definition header : Type :=
  list string.

Definition table : Type :=
  (list row) * (header).

Definition get_rowlist (tbl : table) :=
  match tbl with
  | (rowlist, _) => rowlist
  end.

Definition get_header (tbl: table) :=
  match tbl with
  | (_, h) => h
  end.

Check table.
Check row.

Definition empty_table : table 
    := (@nil (row), nil).

Definition add_header (h : header) (tbl: table) :=
  match tbl with
  | (rowlist, old_h) => match rowlist with
                        | [] => (rowlist, h)
                        | _ => (rowlist, old_h)
                        end
  end.
Definition entry_type_match (e1 : entry) (e2: entry) :=
  match e1, e2 with
  | string_entry _, string_entry _ => true
  | nat_entry _, nat_entry _ => true
  | nil_entry, _ => true
  | _, nil_entry => true
  | _, _ => false
  end.

Definition entry_type_match_prop (e1 : entry) (e2: entry) :=
  match e1, e2 with
  | string_entry _, string_entry _ => True
  | nat_entry _, nat_entry _ => True
  | nil_entry, _ => True
  | _, nil_entry => True
  | _, _ => False
  end.

Fixpoint row_validity_2_3_bool (r1 : row) (r2: row) :=
  match r1, r2 with
  | [],[] => true
  | e1::tl1, e2::tl2 => andb (entry_type_match e1 e2) (row_validity_2_3_bool tl1 tl2)
  | _, _ => false
  end.

Fixpoint row_validity_2_3 (r1 : row) (r2: row) :=
  match r1, r2 with
  | [],[] => True
  | e1::tl1, e2::tl2 => (entry_type_match_prop e1 e2) /\ (row_validity_2_3 tl1 tl2)
  | _, _ => False
  end.

Definition row_validity_2_3_table (r : row) (rowlist : list row) :=
  match rowlist with
  | [] => true
  | first_row::row_tail => row_validity_2_3_bool first_row r
  end.

Definition header_matches_first_row (h : header) (r : row) (rowlist : list row) :=
  match rowlist with
  | [] => List.length h =? List.length r
  | _ => true
  end.

Definition add_row (r : row) (tbl : table) :=
  match tbl with
    | (_, []) => tbl
    | (t, ident) => if
        (andb (header_matches_first_row ident r t) (row_validity_2_3_table r t))
      then ((r::t, ident)) else ((t, ident))
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

(** [remove_first_row_from_row_list r row_list] is row_list with the first row that is equal to r from [row_list] removed **)
Fixpoint remove_first_row_from_row_list (r: row) (row_list : list row) :=
  match row_list with
  | [] => []
  | r_elt :: row_list_tl => if (row_eqb r r_elt) then (row_list_tl)
                            else (remove_first_row_from_row_list r
                                                                 row_list_tl)
  end.

(** [remove_all_row_from_row_list r row_list] is a rowlist with all the rows that are equal to r from [row_list] removed **)
Fixpoint remove_all_row_from_row_list (r : row) (row_list : list row) :=
  filter (fun rw => negb (row_eqb rw r)) row_list.

(** [remove_all_row_from_table r tbl] is a Table with all the rows that are equal to r from [tbl] removed **)
Definition remove_all_row_from_table (r : row) (tbl : table) :=
  match tbl with
  | (row_list, hdr) => ((remove_all_row_from_row_list r row_list), hdr)  
  end.

(** [remove_first_row_from_table r tbl] is a Table with the first row that is equal to r from [tbl] removed **)
Definition remove_first_row_from_table (r : row) (tbl : table) :=
  match tbl with
  | (row_list, hdr) => ((remove_first_row_from_row_list r row_list), hdr)
  end.

(** [filter_row_by_entry f hdr h r] gives true if, for the element e matching the header identifier hdr, [f e = true], otherwise it gives false
    f is the filter function  **)
Fixpoint filter_row_by_entry (f: entry -> bool) (hdr: string) (h: header) (r: row) : bool :=
  match h, r with
  | [], [] => false
  | identifier::t, ent::row_tail => if (string_dec hdr identifier) then (f ent) else (filter_row_by_entry f hdr t row_tail)
  | _, _ => false (** some sort of error with h and r **)
  end.
(** [filter_table_by_entry helper f hdr t h] is a helper for filter_table_by_entry. It has the same semantics but takes 
list rows and headers and gives a [list row] instead of a Table **)
Fixpoint filter_table_by_entry_helper (f: entry -> bool) (hdr: string) (t: list row) (h: header) : list row :=
  match t with
  | [] => []
  | r::tl => if (filter_row_by_entry f hdr h r) then (r::(filter_table_by_entry_helper f hdr tl h)) else ((filter_table_by_entry_helper f hdr tl h)) 
  end.

(** [filter_table_by_entry f hdr tbl] gives a tbl with rows only such that [f e = true] and e is under header element [hdr]
[filter_table_by_entry (fun e => string_eqb e "Haram") "Name" tbl] will return a table from the rows of [tbl] where the "Name" is Haram provided "Name" is 
an element of the header and "Haram" is under the correct "Name" header 

  As demonstrated by the example [f] is the filter function **) 
Fixpoint filter_table_by_entry (f: entry -> bool) (hdr : string) (tbl : table) : table :=
  match tbl with
  | (t, h) => ((filter_table_by_entry_helper f hdr t h), h)
  end.

(** Evidence for table_valid_1 tbl can be given in 3 ways. Constructor same_length requires that the header and the table have the same length.
 Constructor empty_header_valid requires the header to be empty and the rowlist to be empty. Constructor empty_rowlist_valid requires the rowlist to be empty *)  
Inductive table_valid_1 (tbl : table) : Prop :=
| same_length (h : header) (fst_row : list entry) (rowlist : list row) :
    get_header tbl = h -> get_rowlist tbl = rowlist -> List.length h = List.length fst_row ->  (exists tl, rowlist = fst_row::tl) -> table_valid_1 tbl
| empty_header_valid (h : header) (rowlist : list row) :
    get_header tbl = h -> get_rowlist tbl = rowlist -> h = [] -> rowlist = [] -> table_valid_1 tbl
| empty_rowlist_valid (rowlist : list row) : 
	get_rowlist tbl = rowlist -> rowlist = [] -> table_valid_1 tbl.

(** all_rows_same_length rowlist is True iff all the rows in the rowlist are the same length. Empty rowlists and rowlists of only one row are True under
all_rows_same_length **)
Fixpoint all_rows_same_length (rowlist: list row) : Prop :=
  match rowlist with
  | [] => True
  | h::[] => True
  | h1::tl => match tl with
              | [] => True
              | h2::tl2 => ((List.length h1) =
                            (List.length h2)) /\ (all_rows_same_length tl)
              end
  end.

(** table_valid_2 tbl is True iff all the rows in the rowlist of tbl are the same length **)
Inductive table_valid_2 (tbl : table) : Prop :=
| rows_same_length (rowlist : list row) :
    get_rowlist tbl = rowlist -> all_rows_same_length (rowlist) -> table_valid_2 tbl.

(** all_cols_same_type rowlist is True iff all cols in the rowlist have the same type, empty rowlists and rowlists are True under all_cols_same_type **)
Fixpoint all_cols_same_type (rowlist : list row) : Prop :=
  match rowlist with
  | [] => True
  | h::[] => True
  | h1::tl => match tl with
              | [] => True
              | h2::tl2 => (row_validity_2_3 h1 h2) /\ (all_cols_same_type tl)
              end
  end.

(** table_valid_3 tbl is True iff all the cols in the rowlist have the same type**)
Inductive table_valid_3 (tbl : table) : Prop :=
| cols_same_type (rowlist : list row) : get_rowlist tbl = rowlist -> all_cols_same_type (rowlist) -> table_valid_3 tbl.

(** table_valid tbl is True iff table_valid_1 tbl, tbl_valid_2 tbl, and tbl_valid_3 tbl **)
Inductive table_valid (tbl : table) : Prop :=
| valid_1_2_3 : table_valid_1 tbl -> table_valid_2 tbl -> table_valid_3 tbl ->
                table_valid tbl.






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

Theorem add_header_table_is_valid : forall (h : header) (tbl: table),
    table_valid tbl -> table_valid (add_header h tbl).
Proof.
  induction h.
  + induction tbl.
    ++ intros H. apply valid_1_2_3.
       +++ simpl. destruct a eqn:Ha.
           ++++ apply empty_header_valid with (h:=[]) (rowlist:=[]); try (reflexivity).
           ++++ induction b.
  - destruct H. apply H.
  - destruct H. apply H.
    +++ destruct H. destruct a.
        ++++ simpl. apply rows_same_length with (rowlist := []).
             reflexivity. simpl. trivial.
        ++++ simpl. apply H0.
    +++ destruct H. destruct a.
        ++++ simpl. apply cols_same_type with (rowlist:=[]).
  - simpl. reflexivity.
  - simpl. trivial.
    ++++ simpl. apply H1.
  + intros tbl. induction tbl.
    ++ intros H. apply valid_1_2_3.
       +++ destruct H. destruct a0 eqn:Ha0.
           ++++ simpl. apply empty_rowlist_valid with (rowlist:=[]).
  - reflexivity.
  - reflexivity.
    ++++ simpl. apply H.
    +++ destruct a0 eqn:Ha0.
  - simpl. apply rows_same_length with (rowlist:=[]).
    -- reflexivity.
    -- simpl. trivial.
  - simpl. destruct H. apply H0.
    +++ destruct a0.
  - simpl. apply cols_same_type with (rowlist:=[]).
    -- reflexivity.
    -- simpl. trivial.
  - simpl. destruct H. apply H1.
Qed.

Theorem empty_table_is_valid : table_valid empty_table.
Proof.
  apply valid_1_2_3. unfold empty_table.
  + apply empty_rowlist_valid with (rowlist:=[]); try (reflexivity).
  + apply rows_same_length with (rowlist:=[]); try (reflexivity).
  + apply cols_same_type with (rowlist:=[]); try (reflexivity).
Qed.

Theorem add_row_table_is_valid : forall (r: row) (tbl : table),
    table_valid tbl -> table_valid (add_row r tbl).
Proof.
  induction r.
  + intros tbl. intros H. induction tbl. simpl.
    destruct (header_matches_first_row b [] a) eqn:Heq.
    ++ destruct (row_validity_2_3_table [] a) eqn:Hrow_valid.
       +++ simpl. destruct b eqn:Hheader. (** destruct on the header **)
           destruct a eqn:Hrowlist.
  - apply H.
  - apply H.
  - destruct a eqn:Hrowlist.
    -- simpl in Heq. discriminate.
    -- simpl in Hrow_valid. destruct r eqn:Hr.
    * destruct H. destruct H; subst. simpl in H3. destruct H4. simpl in H. injection H.
      intros H_l_x. intros H_fst_row. subst. simpl in H3. discriminate.
      simpl in H3. discriminate. simpl in H2.  discriminate H2.
    * simpl in Hrow_valid. discriminate.
      +++ simpl. destruct b eqn:Hheader; apply H.
      ++ simpl. destruct b eqn:Hb; apply H.
    + intros tbl. intros H. induction tbl.
      ++ destruct b.
         +++ simpl. apply H.
         +++ simpl. destruct (header_matches_first_row (s :: b) (a :: r) a0) eqn:Hhead.
             destruct (row_validity_2_3_table (a::r) a0) eqn:Hrow_valid.
  - simpl. apply valid_1_2_3.
    -- apply same_length with (h:=s::b) (fst_row:=(a::r)) (rowlist:=(a::r)::a0); try(reflexivity).
    * 
  - apply valid_1_2_3.
    * apply empty_header_valid with (rowlist:=[[]]) (h:=[]); try (reflexivity).
      ** simpl. reflexivity.
      ** simpl. try (reflexivity).
      * apply rows_same_length with (rowlist:=[]); try (reflexivity).
      * apply cols_same_type with (rowlist:=[]); try (reflexivity).
       

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
  - simpl. destruct (Datatypes.length h =? 0) eqn:Heq.
    + simpl. reflexivity.
    + simpl. reflexivity.
  - simpl. destruct a.
    -- simpl. destruct (string_dec s s). simpl. destruct (row_eqb r r).
    + simpl. destruct (Datatypes.length h =? S (Datatypes.length r)) eqn:Heq.
      ++ simpl. destruct (string_dec s s). simpl. destruct (row_eqb r r). reflexivity.
         reflexivity. simpl. reflexivity.
      ++ simpl. reflexivity.
    + destruct (Datatypes.length h =? S (Datatypes.length r)) eqn:Heq. simpl.
      destruct (string_dec s s). simpl. destruct (row_eqb r r). reflexivity. reflexivity.
      simpl. reflexivity. simpl. reflexivity.
    + destruct n. reflexivity.
    -- simpl. induction n.
    + simpl. destruct (Datatypes.length h =? S (Datatypes.length r)) eqn:Heq. simpl.
      destruct (row_eqb r r). simpl. reflexivity. reflexivity. simpl. reflexivity. 
    +simpl.  destruct (Datatypes.length h =? S (Datatypes.length r)) eqn:Heq. simpl.
     rewrite Nat.eqb_refl. simpl. destruct (row_eqb r r). reflexivity.
     reflexivity. simpl. reflexivity.
     -- destruct (Datatypes.length h =? S (Datatypes.length r)) eqn:Heq. simpl. reflexivity.
        simpl. reflexivity.
Qed.

(** The false filter is like 0 if filtering is mulitplication *)
Lemma name_this : forall (r : row) (h: header) (col_ident : string) (a: entry) (f: entry -> bool),
    f = (fun e => false) -> filter_row_by_entry f col_ident h (a::r) = filter_row_by_entry f col_ident h r.
Proof.
  intros r h col_ident a f H_f.
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
    -- simpl. rewrite Heq.
Abort.
End Table.