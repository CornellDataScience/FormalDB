Require Import List Arith Bool Ascii String.
Require String.
Import ListNotations.
From Coq Require Extraction.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Omega.
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

(** [get_rowlist] is the list of rows from table [tbl] *)
Definition get_rowlist (tbl : table) :=
  match tbl with
  | (rowlist, _) => rowlist
  end.

(** [get_header] is the header of table [tbl] *)
Definition get_header (tbl: table) :=
  match tbl with
  | (_, h) => h
  end.

Check table.
Check row.

Definition empty_table : table 
    := (@nil (row), nil).

(** [add_header] adds header [h] to table [tbl] if [tbl] is empty.
Returns old table if [tbl] is not empty. *)
Definition add_header (h : header) (tbl: table) :=
  match tbl with
  | (rowlist, old_h) => match rowlist with
                        | [] => (rowlist, h)
                        | _ => (rowlist, old_h)
                        end
  end.

(** [entry_type_match] checks if entries [e1] and [e2] are of the same type *)
Definition entry_type_match (e1 : entry) (e2: entry) :=
  match e1, e2 with
  | string_entry _, string_entry _ => true
  | nat_entry _, nat_entry _ => true
  | nil_entry, _ => true
  | _, nil_entry => true
  | _, _ => false
  end.

(** [entry_type_match_prop] checks is entries [e1] and [e2] are of the same
type for propositions *)
Definition entry_type_match_prop (e1 : entry) (e2: entry) :=
  match e1, e2 with
  | string_entry _, string_entry _ => True
  | nat_entry _, nat_entry _ => True
  | nil_entry, _ => True
  | _, nil_entry => True
  | _, _ => False
  end.

(** [row_validity_2_3_bool] takes in row [r1] and [r2] and checks
if the entry in a row is the same as the entry in the row before it
at the same index *)
Fixpoint row_validity_2_3_bool (r1 : row) (r2: row) :=
  match r1, r2 with
  | [],[] => true
  | e1::tl1, e2::tl2 => andb (entry_type_match e1 e2) (row_validity_2_3_bool tl1 tl2)
  | _, _ => false
  end.

(** [row_validity_2_3_bool] takes in row [r1] and [r2] and checks
if the entry in a row is the same as the entry in the row before it
at the same index for propositions *)
Fixpoint row_validity_2_3 (r1 : row) (r2: row) :=
  match r1, r2 with
  | [],[] => True
  | e1::tl1, e2::tl2 => (entry_type_match_prop e1 e2) /\ (row_validity_2_3 tl1 tl2)
  | _, _ => False
  end.

(** [row_validity_2_3_table] checks that every entry in row [r] in the
table with rows [rowlist] is of the same type as the corresponding entry in the
first row in the [rowlist]. *)
Definition row_validity_2_3_table (r : row) (rowlist : list row) :=
  match rowlist with
  | [] => true
  | first_row::row_tail => row_validity_2_3_bool first_row r
  end.

(** [header_matches_first_row] checks that the header [h] in the table
with rows [rowlist] has the same number of entries as row [r]. *)
Definition header_matches_first_row (h : header) (r : row) (rowlist : list row) :=
  match rowlist with
  | [] => List.length h =? List.length r
  | _ => true
  end.

(** [add_row] adds row [r] to table [tbl] and returns the resulting table.
Checks validity of [r] and of the resulting table. *)
Definition add_row (r : row) (tbl : table) :=
  match tbl with
    | (_, []) => tbl
    | (t, ident) => if
        (andb (header_matches_first_row ident r t) (row_validity_2_3_table r t))
      then ((r::t, ident)) else ((t, ident))
    end.

(** [entry_eqb] is the equality of entry [e1] and [e2]. *)
Definition entry_eqb (e1: entry) (e2: entry) :=
  match e1, e2 with
  | string_entry s1, string_entry s2 => if (string_dec s1 s2) then true else false
  | nat_entry n1, nat_entry n2 => n1 =? n2
  | _, _ => false
  end.

(** [row_eqb] is the equality of row [r1] and [r2]. Checks that every entry in
[r1] and is equal to every respective entry in [r2]. *)
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

Theorem empty_table_is_valid : table_valid empty_table.
Proof.
  apply valid_1_2_3. unfold empty_table.
  + apply empty_rowlist_valid with (rowlist:=[]); try (reflexivity).
  + apply rows_same_length with (rowlist:=[]); try (reflexivity).
  + apply cols_same_type with (rowlist:=[]); try (reflexivity).
Qed.

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
Lemma entry_type_match_reflect : forall a a0,
    entry_type_match a a0 = true -> entry_type_match_prop a a0.
Proof.
  induction a.
  + intros. simpl. induction a0. auto. simpl in H. discriminate. auto.
  + induction a0; try (intros; simpl in H; discriminate).
  - intros. simpl. auto.
  - intros. simpl. auto.
    + induction a0; try (intros; simpl in H; discriminate).
  - intros. simpl. auto.
  - intros. simpl. auto.
  - intros. simpl. auto.
Qed.

Lemma rw_validity_2_3_reflect : forall r1 r2,
    row_validity_2_3_bool r1 r2 = true -> row_validity_2_3 r1 r2.
Proof.
  induction r1.
  + intros. simpl. induction r2. auto. simpl in H. discriminate.
  + intros. simpl. generalize dependent r1. induction r2. intros. discriminate H.
    intros. simpl in H.
    rewrite andb_true_iff in H. destruct H. split.
    -- pose proof entry_type_match_reflect as He. apply He in H. apply H.
    -- apply IHr1. apply H0.
Qed.
                                               
Lemma row_validity_2_3_bool_is_valid : forall r1 r2,
    row_validity_2_3_bool r1 r2 = true-> all_rows_same_length (r1::r2::[]) /\
    all_cols_same_type (r1::r2::[]).
Proof.
  induction r1.
  + intros r2. intros H. split.
    ++ simpl. split. destruct r2. simpl in H. simpl. reflexivity. simpl in H. discriminate.
       auto.
    ++ simpl. split. destruct r2. auto. simpl in H. discriminate. auto.
  + intros r2. intros H. split. simpl. split. simpl in H. generalize dependent r1. induction r2.
  - simpl. intros. discriminate.
  - intros. Search (_ && _ = true). rewrite andb_true_iff in H.
    destruct H. apply IHr1 in H0. destruct H0. simpl in H0. destruct H0.
    simpl. Search (S _ = S _). rewrite Nat.succ_inj_wd. apply H0.
  - trivial.
  - simpl. split. generalize dependent r1. induction r2.
    -- intros. simpl in H. discriminate.
    -- intros. split. subst. simpl in H. rewrite andb_true_iff in H.
       destruct H. induction a.
    * simpl. induction a0.
      ** auto.
      ** simpl in H. discriminate.
      ** auto.
    * simpl. induction a0; try (auto).
      ** simpl in H. discriminate.
    * simpl. induction a0; try (auto).
    * simpl in H. rewrite andb_true_iff in H. destruct H.
      apply rw_validity_2_3_reflect in H0. apply H0.
      -- auto.
Qed.
Print row_validity_2_3_table.
Print table.
Lemma entry_type_match_commutes : forall e0 e1,
    entry_type_match_prop e0 e1 -> entry_type_match_prop e1 e0.
Proof.
  induction e0.
  + induction e1. intros. simpl. trivial. simpl. trivial. simpl. trivial.
  + induction e1; intros; try (simpl; trivial).
  + induction e1; intros; try (simpl; trivial).
Qed.
Lemma row_validity_commutes : forall r1 r2,
    row_validity_2_3 r1 r2 -> row_validity_2_3 r2 r1.
Proof.
  intros r1 r2 H.
  generalize dependent r2.
  induction r1.
  + intros r2. induction r2. simpl. trivial. intros.
    simpl in H. destruct H.
  + intros r2. induction r2. intros H. simpl. simpl in H. destruct H.
    simpl. intros. destruct H. split.
  - pose proof entry_type_match_commutes. apply H1 in H. apply H.
  - apply IHr1. apply H0.
Qed.
    
Lemma row_validity_2_3_table_is_valid : forall h r1 rowlist,
    table_valid (rowlist,h) -> row_validity_2_3_table r1 (rowlist) = true ->
    rowlist <> [] -> table_valid (r1::rowlist, h).
Proof.
  intros.
  constructor.
  + induction rowlist.
    * contradiction.
    * simpl in H0. pose proof row_validity_2_3_bool_is_valid. apply H2 in H0.
      destruct H0. apply same_length with (rowlist:=(r1::a::rowlist)) (h:=h) (fst_row:=r1).
      auto. auto. destruct H. destruct H. simpl in H. simpl in H6. subst. destruct H8.
      injection H. intros. symmetry in H8. subst. rewrite H7. destruct H0. apply H0.
      subst. simpl in H8. contradiction. simpl in H. subst. contradiction.
      exists (a::rowlist). reflexivity.
  + apply rows_same_length with (rowlist:=r1::rowlist). auto.
    pose proof row_validity_2_3_bool_is_valid. induction rowlist.
    * contradiction.
    * apply H2 in H0. destruct H0. destruct H0. remember H. destruct H. destruct t1.
      simpl in e. subst. split. rewrite H0. reflexivity.
      apply a0.
  + apply cols_same_type with (rowlist:=r1::rowlist).
    ++ induction rowlist.
    * contradiction.
    * auto.
    ++  pose proof row_validity_2_3_bool_is_valid. induction rowlist.
    * contradiction.
    * split. simpl in H0. pose proof rw_validity_2_3_reflect.
      apply H3 in H0. pose proof row_validity_commutes.  apply H4 in H0.
      apply H0. destruct H. destruct H4. simpl in H4. subst. apply H5.
Qed.

Lemma beq_reflect : forall  x y, reflect (x = y) (x =? y).
Proof.
  intros x y.
  apply iff_reflect. symmetry. apply beq_nat_true_iff.
Qed.
Lemma blt_reflect : forall x y, reflect (x < y) (x <? y).
Proof.
  intros x y.
  apply iff_reflect. symmetry. apply Nat.ltb_lt.
Qed.
Lemma ble_reflect : forall  x y, reflect (x <= y) (x <=? y).
Proof.
  intros x y.
  apply iff_reflect. symmetry. apply Nat.leb_le.
Qed.

Hint Resolve blt_reflect ble_reflect beq_reflect : bdestruct.

Ltac bdestruct X :=
  let H := fresh in let e := fresh "e" in
   evar (e: Prop);
   assert (H: reflect e X); subst e;
    [eauto with bdestruct
    | destruct H as [H|H];
       [ | try first [apply not_lt in H | apply not_le in H]]].

Theorem add_row_table_is_valid : forall (r: row) (tbl: table),
    table_valid tbl -> table_valid (add_row r tbl).
Proof.
  intros r tbl.
  generalize dependent r.
  destruct tbl.
  generalize dependent h.
  induction l.
  + intros. simpl. bdestruct (Datatypes.length h =? Datatypes.length r). simpl.
    destruct h. apply H.
    constructor.
    * destruct H. apply same_length with (h:=(s::h)) (fst_row:=r) (rowlist:=[r]).
      auto. auto. apply H0. exists []. reflexivity.
    * destruct H. destruct H1. simpl in H1. subst.
      apply rows_same_length with (rowlist:=[r]). auto. auto.
    * apply cols_same_type with (rowlist:=[r]). auto. simpl. trivial.
    * simpl. destruct h. apply H. apply H.
  + intros. simpl. destruct h. apply H.
    destruct (row_validity_2_3_bool a r) eqn:Hr.
    pose proof row_validity_2_3_table_is_valid.
    apply H0. apply H. simpl. apply Hr. unfold not. intros. discriminate.
    apply H.
Qed.

Theorem filter_mantains_header : forall f ident h rowlist h' rowlist',
    filter_table_by_entry f ident (rowlist, h) = (rowlist', h') -> h = h'.
Proof.
  intros.
  destruct ident.
  simpl in H. injection H. intros. apply H0.
  simpl in H. injection H. intros. apply H0.
Qed.

Inductive table_constructed (tbl: table) := 
| empty_constructed : tbl = empty_table -> table_constructed tbl
| add_header_constructed (sub_tbl : table) (h : header) :  table_constructed sub_tbl ->
                                           add_header h sub_tbl = tbl ->
                                           table_constructed tbl
| add_row_constructed (sub_tbl : table) (r: row) : table_constructed sub_tbl ->
                                        add_row r sub_tbl = tbl ->
                                        table_constructed tbl.
                                                          
Theorem remove_added_row_is_valid : forall r tbl,
    table_valid (add_row r tbl) -> table_valid tbl.
Proof.
  intros.
  pose proof add_row_table_is_valid.
  constructor.
  + destruct tbl. destruct h. simpl in H. apply H.
    simpl in H. destruct (header_matches_first_row (s::h) r l) eqn:Heq.
    destruct (row_validity_2_3_table r l) eqn:Heq2. simpl in H.
    destruct l. simpl in Heq. destruct (Datatypes.length r




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

End Table.

Extract Inductive bool ⇒ "bool" [ "true" "false" ].
Extract Inductive nat ⇒ "int"
  [ "0" "(fun x → x + 1)" ]
  "(fun zero succ n →
      if n=0 then zero () else succ (n-1))".
Extract Constant plus ⇒ "( + )".
Extract Constant mult ⇒ "( * )".
Extract Constant eqb ⇒ "( = )".
Extraction "Table2.ml" Table.