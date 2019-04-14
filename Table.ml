
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type bool =
| True
| False

(** val negb : bool -> bool **)

let negb = function
| True -> False
| False -> True

type nat =
| O
| S of nat

type ('a, 'b) prod =
| Pair of 'a * 'b

type 'a list =
| Nil
| Cons of 'a * 'a list

(** val length : 'a1 list -> nat **)

let rec length = function
| Nil -> O
| Cons (_, l') -> S (length l')

type sumbool =
| Left
| Right

(** val bool_dec : bool -> bool -> sumbool **)

let bool_dec b1 b2 =
  match b1 with
  | True -> (match b2 with
             | True -> Left
             | False -> Right)
  | False -> (match b2 with
              | True -> Right
              | False -> Left)

module Nat =
 struct
  (** val eqb : nat -> nat -> bool **)

  let rec eqb n m =
    match n with
    | O -> (match m with
            | O -> True
            | S _ -> False)
    | S n' -> (match m with
               | O -> False
               | S m' -> eqb n' m')
 end

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| Nil -> Nil
| Cons (x, l0) -> (match f x with
                   | True -> Cons (x, (filter f l0))
                   | False -> filter f l0)

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

(** val ascii_dec : ascii -> ascii -> sumbool **)

let ascii_dec a b =
  let Ascii (x, x0, x1, x2, x3, x4, x5, x6) = a in
  let Ascii (b8, b9, b10, b11, b12, b13, b14, b15) = b in
  (match bool_dec x b8 with
   | Left ->
     (match bool_dec x0 b9 with
      | Left ->
        (match bool_dec x1 b10 with
         | Left ->
           (match bool_dec x2 b11 with
            | Left ->
              (match bool_dec x3 b12 with
               | Left ->
                 (match bool_dec x4 b13 with
                  | Left ->
                    (match bool_dec x5 b14 with
                     | Left -> bool_dec x6 b15
                     | Right -> Right)
                  | Right -> Right)
               | Right -> Right)
            | Right -> Right)
         | Right -> Right)
      | Right -> Right)
   | Right -> Right)

type string =
| EmptyString
| String of ascii * string

(** val string_dec : string -> string -> sumbool **)

let rec string_dec s x =
  match s with
  | EmptyString -> (match x with
                    | EmptyString -> Left
                    | String (_, _) -> Right)
  | String (a, s0) ->
    (match x with
     | EmptyString -> Right
     | String (a0, s1) ->
       (match ascii_dec a a0 with
        | Left -> string_dec s0 s1
        | Right -> Right))

module Table =
 struct
  (** val string_eq : string -> string -> bool **)

  let string_eq s1 s2 =
    match string_dec s1 s2 with
    | Left -> True
    | Right -> False

  type entry =
  | Coq_string_entry of string
  | Coq_nat_entry of nat
  | Coq_nil_entry

  (** val entry_rect : (string -> 'a1) -> (nat -> 'a1) -> 'a1 -> entry -> 'a1 **)

  let entry_rect f f0 f1 = function
  | Coq_string_entry x -> f x
  | Coq_nat_entry x -> f0 x
  | Coq_nil_entry -> f1

  (** val entry_rec : (string -> 'a1) -> (nat -> 'a1) -> 'a1 -> entry -> 'a1 **)

  let entry_rec f f0 f1 = function
  | Coq_string_entry x -> f x
  | Coq_nat_entry x -> f0 x
  | Coq_nil_entry -> f1

  type row = entry list

  type header = string list

  type table = (row list, header) prod

  (** val get_rowlist : table -> row list **)

  let get_rowlist = function
  | Pair (rowlist, _) -> rowlist

  (** val get_header : table -> header **)

  let get_header = function
  | Pair (_, h) -> h

  (** val empty_table : table **)

  let empty_table =
    Pair (Nil, Nil)

  (** val add_header : header -> table -> (row list, header) prod **)

  let add_header h = function
  | Pair (rowlist, old_h) ->
    (match rowlist with
     | Nil -> Pair (rowlist, h)
     | Cons (_, _) -> Pair (rowlist, old_h))

  (** val entry_type_match : entry -> entry -> bool **)

  let entry_type_match e1 e2 =
    match e1 with
    | Coq_string_entry _ -> (match e2 with
                             | Coq_nat_entry _ -> False
                             | _ -> True)
    | Coq_nat_entry _ -> (match e2 with
                          | Coq_string_entry _ -> False
                          | _ -> True)
    | Coq_nil_entry -> True

  (** val row_validity_2_3_bool : row -> row -> bool **)

  let rec row_validity_2_3_bool r1 r2 =
    match r1 with
    | Nil -> (match r2 with
              | Nil -> True
              | Cons (_, _) -> False)
    | Cons (e1, tl1) ->
      (match r2 with
       | Nil -> False
       | Cons (e2, tl2) ->
         (match entry_type_match e1 e2 with
          | True -> row_validity_2_3_bool tl1 tl2
          | False -> False))

  (** val row_validity_2_3_table : row -> row list -> bool **)

  let row_validity_2_3_table r = function
  | Nil -> True
  | Cons (first_row, _) -> row_validity_2_3_bool first_row r

  (** val header_matches_first_row : header -> row -> row list -> bool **)

  let header_matches_first_row h r = function
  | Nil -> Nat.eqb (length h) (length r)
  | Cons (_, _) -> True

  (** val add_row : row -> table -> table **)

  let add_row r tbl = match tbl with
  | Pair (t, ident) ->
    (match ident with
     | Nil -> tbl
     | Cons (_, _) ->
       (match match header_matches_first_row ident r t with
              | True -> row_validity_2_3_table r t
              | False -> False with
        | True -> Pair ((Cons (r, t)), ident)
        | False -> Pair (t, ident)))

  (** val entry_eqb : entry -> entry -> bool **)

  let entry_eqb e1 e2 =
    match e1 with
    | Coq_string_entry s1 ->
      (match e2 with
       | Coq_string_entry s2 -> (match string_dec s1 s2 with
                                 | Left -> True
                                 | Right -> False)
       | _ -> False)
    | Coq_nat_entry n1 -> (match e2 with
                           | Coq_nat_entry n2 -> Nat.eqb n1 n2
                           | _ -> False)
    | Coq_nil_entry -> False

  (** val row_eqb : row -> row -> bool **)

  let rec row_eqb r1 r2 =
    match r1 with
    | Nil -> (match r2 with
              | Nil -> True
              | Cons (_, _) -> False)
    | Cons (e1, tl1) ->
      (match r2 with
       | Nil -> False
       | Cons (e2, tl2) ->
         (match entry_eqb e1 e2 with
          | True -> row_eqb tl1 tl2
          | False -> False))

  (** val remove_first_row_from_row_list : row -> row list -> row list **)

  let rec remove_first_row_from_row_list r = function
  | Nil -> Nil
  | Cons (r_elt, row_list_tl) ->
    (match row_eqb r r_elt with
     | True -> row_list_tl
     | False -> remove_first_row_from_row_list r row_list_tl)

  (** val remove_all_row_from_row_list : row -> row list -> row list **)

  let rec remove_all_row_from_row_list r row_list =
    filter (fun rw -> negb (row_eqb rw r)) row_list

  (** val remove_all_row_from_table : row -> table -> (row list, header) prod **)

  let remove_all_row_from_table r = function
  | Pair (row_list, hdr) -> Pair ((remove_all_row_from_row_list r row_list), hdr)

  (** val remove_first_row_from_table : row -> table -> (row list, header) prod **)

  let remove_first_row_from_table r = function
  | Pair (row_list, hdr) -> Pair ((remove_first_row_from_row_list r row_list), hdr)

  (** val filter_row_by_entry : (entry -> bool) -> string -> header -> row -> bool **)

  let rec filter_row_by_entry f hdr h r =
    match h with
    | Nil -> False
    | Cons (identifier, t) ->
      (match r with
       | Nil -> False
       | Cons (ent, row_tail) ->
         (match string_dec hdr identifier with
          | Left -> f ent
          | Right -> filter_row_by_entry f hdr t row_tail))

  (** val filter_table_by_entry_helper :
      (entry -> bool) -> string -> row list -> header -> row list **)

  let rec filter_table_by_entry_helper f hdr t h =
    match t with
    | Nil -> Nil 
    | Cons (r, tl) ->
      (match filter_row_by_entry f hdr h r with
       | True -> Cons (r, (filter_table_by_entry_helper f hdr tl h))
       | False -> filter_table_by_entry_helper f hdr tl h)

  (** val filter_table_by_entry : (entry -> bool) -> string -> table -> table **)

  let rec filter_table_by_entry f hdr = function
  | Pair (t, h) -> Pair ((filter_table_by_entry_helper f hdr t h), h)

  (** val table_valid_rect : table -> (__ -> __ -> __ -> 'a1) -> 'a1 **)

  let table_valid_rect _ f =
    f __ __ __

  (** val table_valid_rec : table -> (__ -> __ -> __ -> 'a1) -> 'a1 **)

  let table_valid_rec _ f =
    f __ __ __

  (** val entry_is_string : string -> entry -> bool **)

  let entry_is_string s = function
    | Coq_string_entry s_e -> (match string_dec s_e s with
        | Left -> True
        | Right -> False)
    | _ -> False
end
