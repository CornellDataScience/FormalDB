open CSV
open Table2
(* HOPEFULLY THIS WILL BE IMPLEMENTED WITH A REGEX TOKENIZER *)
type command =
  | Exit
  | SQL of SQL
  | Print
  | Malformed


type SQL =
  |Create of (string * string) list
  |Select of (string * Columns * Conditions)
  |Update of (string * Columns * Table2.entry list * Conditions)
  |Delete of (string * Conditions)
  |Insert of (string * Columns * Table2.entry list)

type Conditions =
  |ColVal of (string * string)
  |AND of (Conditions * Conditions)
  |NOT of Conditions
  |OR of (Conditions * Conditions)
  |GT of (Conditions * Conditions)
  |LT of (Conditions * Conditions)

type Columns =
  |Nil
  |Col of (string * Columns)

let parse_select cmd =
   cmd

(*  parses the columns to be created for a SQL create command*)
let parse_create_cols col =
  match col with
  |h::t -> if (h != '(') then Malformed else parse_create_helper t []

(*  NOT CORRECT, NEED TO THINK MORE ABOUT TYPES*)
(* returns the type of the SQL column in our types *)
let type_of col_type =
  match col_type with
  |"int" -> Coq_nat_entry
  |_ -> Coq_string_entry

(*  Helper for the parse create*)
let rec parse_create_helper col lst =
  begin
  match col with
  |col_name::col_type::t -> if (col_type.[(length col_type) - 1] != ',') then begin
      match t with
      |paren::semicol::[] -> if (paren == ')' && semicol == ';') then Some lst::(col_name, (type_of col_type)) else None
      |_ -> None
      end
    else parse_create_helper t (lst::(col_name, (type_of col_type)))
  end

(* Parses the SQL create command *)
let parse_create cmd =
  match cmd with
  |[] -> Malformed
  |_::[] -> Malformed
  |h1::h2::t -> match h1 with
    (*  THIS CREATES A TABLE OF NAME  t*)
    |"table" -> create_tbl h2 (parse_create_cols t)
    |_ -> Malformed

let parse_conditions conditions =
  conditions

let parse_update cmd =
  cmd

let parse_delete cmd =
  cmd

let parse_insert cmd =
begin
  match cmd with
  |[] -> Malformed
  |_:[] -> Malformed
  |h1::h2::t -> begin
      match h1 with
      |"into" ->
  end
end

let parse (cmd) =
  let comd = List.map (String.lowercase_ascii) cmd in
  match ((String.split_on_char ' ' comd): string list) with
  | [] -> Malformed
  | h::[] -> begin
      match h with
      | "print" -> Print
      | "exit" -> Exit
      | _ -> Malformed
    end
  | h::t -> begin
      match h with
      |"create" -> parse_create t
      |"select" -> parse_select t
      |"update" -> parse_update t
      |"delete" -> parse_delete t
      |"insert" -> parse_insert t
    end
