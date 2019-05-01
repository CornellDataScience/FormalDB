type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> (fun x -> x + 1) (length l')

module Nat =
 struct
  (** val eqb : int -> int -> bool **)

  let rec eqb n m =
    (fun zero succ n ->       if n=0 then zero () else succ (n-1))
      (fun _ ->
      (fun zero succ n ->       if n=0 then zero () else succ (n-1))
        (fun _ -> true)
        (fun _ -> false)
        m)
      (fun n' ->
      (fun zero succ n ->       if n=0 then zero () else succ (n-1))
        (fun _ -> false)
        (fun m' -> eqb n' m')
        m)
      n
 end

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| [] -> []
| x :: l0 -> if f x then x :: (filter f l0) else filter f l0

(** val string_dec : char list -> char list -> bool **)

let rec string_dec s x =
  match s with
  | [] -> (match x with
           | [] -> true
           | _::_ -> false)
  | a::s0 ->
    (match x with
     | [] -> false
     | a0::s1 -> if (=) a a0 then string_dec s0 s1 else false)

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

module Table2 =
 struct
  (** val string_eq : char list -> char list -> bool **)

  let string_eq s1 s2 =
    if string_dec s1 s2 then true else false

  type entry =
  | Coq_string_entry of char list
  | Coq_nat_entry of float
  | Coq_nil_entry

  (** val entry_rect : (char list -> 'a1) -> (int -> 'a1) -> 'a1 -> entry -> 'a1 **)

  let entry_rect f f0 f1 = function
  | Coq_string_entry x -> f x
  | Coq_nat_entry x -> f0 x
  | Coq_nil_entry -> f1

  (** val entry_rec : (char list -> 'a1) -> (int -> 'a1) -> 'a1 -> entry -> 'a1 **)

  let entry_rec f f0 f1 = function
  | Coq_string_entry x -> f x
  | Coq_nat_entry x -> f0 x
  | Coq_nil_entry -> f1

  type row = entry list

  type header = char list list

  type table = row list * header

  (** val get_rowlist : table -> row list **)

  let get_rowlist = function
  | (rowlist, _) -> rowlist

  (** val get_header : table -> header **)

  let get_header = function
  | (_, h) -> h

  (** val empty_table : table **)

  let empty_table =
    ([], [])

  (** val add_header : header -> table -> row list * header **)

  let add_header h = function
  | (rowlist, old_h) ->
    (match rowlist with
     | [] -> (rowlist, h)
     | _ :: _ -> (rowlist, old_h))

  (** val entry_type_match : entry -> entry -> bool **)

  let entry_type_match e1 e2 =
    match e1 with
    | Coq_string_entry _ -> (match e2 with
                             | Coq_nat_entry _ -> false
                             | _ -> true)
    | Coq_nat_entry _ -> (match e2 with
                          | Coq_string_entry _ -> false
                          | _ -> true)
    | Coq_nil_entry -> true

  (** val row_validity_2_3_bool : row -> row -> bool **)

  let rec row_validity_2_3_bool r1 r2 =
    match r1 with
    | [] -> (match r2 with
             | [] -> true
             | _ :: _ -> false)
    | e1 :: tl1 ->
      (match r2 with
       | [] -> false
       | e2 :: tl2 -> (&&) (entry_type_match e1 e2) (row_validity_2_3_bool tl1 tl2))

  (** val row_validity_2_3_table : row -> row list -> bool **)

  let row_validity_2_3_table r = function
  | [] -> true
  | first_row :: _ -> row_validity_2_3_bool first_row r

  (** val header_matches_first_row : header -> row -> row list -> bool **)

  let header_matches_first_row h r = function
  | [] -> Nat.eqb (length h) (length r)
  | _ :: _ -> true

  (** val add_row : row -> table -> table **)

  let add_row r tbl = match tbl with
  | (t, ident) ->
    (match ident with
     | [] -> tbl
     | _ :: _ ->
       if (&&) (header_matches_first_row ident r t) (row_validity_2_3_table r t)
       then ((r :: t), ident)
       else (t, ident))

  (** val entry_eqb : entry -> entry -> bool **)

  let entry_eqb e1 e2 =
    match e1 with
    | Coq_string_entry s1 ->
      (match e2 with
       | Coq_string_entry s2 -> if string_dec s1 s2 then true else false
       | _ -> false)
    | Coq_nat_entry n1 -> (match e2 with
                           | Coq_nat_entry n2 -> (=) n1 n2
                           | _ -> false)
    | Coq_nil_entry -> false

  (** val row_eqb : row -> row -> bool **)

  let rec row_eqb r1 r2 =
    match r1 with
    | [] -> (match r2 with
             | [] -> true
             | _ :: _ -> false)
    | e1 :: tl1 ->
      (match r2 with
       | [] -> false
       | e2 :: tl2 -> (&&) (entry_eqb e1 e2) (row_eqb tl1 tl2))

  (** val remove_first_row_from_row_list : row -> row list -> row list **)

  let rec remove_first_row_from_row_list r = function
  | [] -> []
  | r_elt :: row_list_tl ->
    if row_eqb r r_elt then row_list_tl else remove_first_row_from_row_list r row_list_tl

  (** val remove_all_row_from_row_list : row -> row list -> row list **)

  let rec remove_all_row_from_row_list r row_list =
    filter (fun rw -> negb (row_eqb rw r)) row_list

  (** val remove_all_row_from_table : row -> table -> row list * header **)

  let remove_all_row_from_table r = function
  | (row_list, hdr) -> ((remove_all_row_from_row_list r row_list), hdr)

  (** val remove_first_row_from_table : row -> table -> row list * header **)

  let remove_first_row_from_table r = function
  | (row_list, hdr) -> ((remove_first_row_from_row_list r row_list), hdr)

  (** val filter_row_by_entry : (entry -> bool) -> char list -> header -> row -> bool **)

  let rec filter_row_by_entry f hdr h r =
    match h with
    | [] -> false
    | identifier :: t ->
      (match r with
       | [] -> false
       | ent :: row_tail ->
         if string_dec hdr identifier then f ent else filter_row_by_entry f hdr t row_tail)

  (** val filter_table_by_entry_helper :
      (entry -> bool) -> char list -> row list -> header -> row list **)

  let rec filter_table_by_entry_helper f hdr t h =
    match t with
    | [] -> []
    | r :: tl ->
      if filter_row_by_entry f hdr h r
      then r :: (filter_table_by_entry_helper f hdr tl h)
      else filter_table_by_entry_helper f hdr tl h

  (** val filter_table_by_entry : (entry -> bool) -> char list -> table -> table **)

  let rec filter_table_by_entry f hdr = function
  | (t, h) -> ((filter_table_by_entry_helper f hdr t h), h)

  (** val table_valid_rect : table -> (__ -> __ -> __ -> 'a1) -> 'a1 **)

  let table_valid_rect _ f =
    f __ __ __

  (** val table_valid_rec : table -> (__ -> __ -> __ -> 'a1) -> 'a1 **)

  let table_valid_rec _ f =
    f __ __ __

  let rec print_header h = 
  match h with 
  | [] -> ()
  | head::t -> print_string (implode(head) ^ "\t"); print_header t

  let rec print_row r =
  match r with 
  | [] -> ()
  | h::t -> match h with 
            | Coq_nat_entry n -> print_float n; print_string "\t"; print_row t
            | Coq_nil_entry -> print_string ("[NULL_ENTRY]" ^ "\t"); print_row t
            | Coq_string_entry s -> print_string (implode(s) ^ "\t"); print_row t

  let rec print_table tbl = 
    let rec print_rowlist rwlst = 
      match rwlst with 
      | [] -> ()
      | head::t -> (print_row head); (print_string "\n"); (print_rowlist t)
    in

    let rowlist = get_rowlist tbl in
    let hdr = get_header tbl in 
    print_header hdr; print_string "\n";
    print_rowlist rowlist

  (** val test_tbl : row list * header **)

  let test_tbl =
    add_header
      (('N'::('a'::('m'::('e'::[])))) :: (('Y'::('e'::('a'::('r'::[])))) :: (('A'::('d'::('d'::('r'::('e'::('s'::('s'::[]))))))) :: (('M'::('a'::('j'::('o'::('r'::[]))))) :: []))))
      empty_table

  (** val test_tbl_1 : table **)

  

  (** val entry_is_haram : entry -> bool **)

  let entry_is_haram = function
  | Coq_string_entry s ->
    if string_dec s ('H'::('a'::('r'::('a'::('m'::[]))))) then true else false
  | _ -> false

  (** val entry_is_string : char list -> entry -> bool **)

  let entry_is_string s = function
  | Coq_string_entry s_e -> if string_dec s_e s then true else false
  | _ -> false
 end
