(* parser_tests.ml *)
(* Run various test cases to verify the Parser module *)

open Parser
open Printf

let rec printingThing thing =
  match thing with
  | Closure _ -> printf "[Closure]"
  | Primitive _ -> printf "[Primitive]"
  | Nil -> printf "nil"
  | Number n -> printf "%d" n
  | Symbol s -> printf "%s" s
  | Cons (_, _) ->
      printf "(";
      printingThings thing;
      printf ")"

and printingThings things =
  match things with
  | Nil -> ()
  | Cons (head, Nil) -> printingThing head
  | Cons (head, tail) ->
      printingThing head;
      printf " ";
      printingThings tail
  | _ -> ()

let printThing thing =
  printingThing thing;
  printf "\n"

(* Test runner *)
let test_file filename =
  try
    Printf.printf "\nTesting file: %s\n" filename;
    Parser.initialize filename;
    let result = Parser.nextThing () in
    printThing result
  with
  | Parser.Can'tParse msg -> Printf.printf "Parse error: %s\n" msg

(* List of test files to run *)
let () =
  let test_files = [
    "factorial.lsp";
    "list1.lsp";
    "nested.lsp";
    "empty.lsp";
    "invalid.lsp"
  ] in
  List.iter test_file test_files
