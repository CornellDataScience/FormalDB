open CSV
open Table2
open Lymp

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

type Columns =
  |Nil
  |Col of (string * Columns)

let parse_select cmd =
  cmd

let parse_create cmd =
  cmd

let parse_update cmd =
  cmd

let parse_delete cmd =
  cmd

let parse_insert cmd =
  cmd

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




let interpreter = "python"
let py = init ~exec:interpreter "."
let parsing = get_module py "parsing"
let parse_sql s = get_string parsing "parse_sql" [Pystr s] in

let main () =
  ANSITerminal.(print_string [red]
                  "\n\n Formal DB Terminal");
  print_endline "Enter SQL Commands \n";
  print_string  "> ";
  match read_line () with
  |exception End_of_file -> ()
  |exception _ -> print_endline "Invalid commands"
  | file_name -> play_game file_name
