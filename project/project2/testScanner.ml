(*
   TEST SCANNER. Test the Lisp lexical scanner.

     James Moen
     07 Apr 25
*)

(* TEST SCANNER. Read TOKENs from the file whose pathname is the string PATH.
   Test the SCANNER by printing those TOKENs one per line. To run this program
   you must type the following to the OCaml toplevel:

     #use "scanner.ml" ;;
     #use "testScanner.ml" ;;

   You must also have the file factorial.lsp in your directory. *)

let testScanner path =
  let rec testing token =
    match token
    with Scanner.CloseParenToken ->
           Printf.printf "CloseParenToken\n" ;
           testing (Scanner.nextToken ()) |

         Scanner.EndToken ->
           Printf.printf "EndToken\n" ;
           () |

         Scanner.NumberToken number ->
           Printf.printf "NumberToken %i\n" number ;
           testing (Scanner.nextToken ()) |

         Scanner.OpenParenToken ->
           Printf.printf "OpenParenToken\n" ;
           testing (Scanner.nextToken ()) |

         Scanner.SymbolToken chars ->
           Printf.printf "SymbolToken \"%s\"\n" chars ;
           testing (Scanner.nextToken ())

  in Scanner.initialize path ;
     testing (Scanner.nextToken ()) ;;

(* Run it! *)

testScanner "factorial.lsp" ;;

(* If SCANNER works, then these lines should be printed:

OpenParenToken
SymbolToken "define"
SymbolToken "!"
OpenParenToken
SymbolToken "lambda"
OpenParenToken
SymbolToken "n"
CloseParenToken
OpenParenToken
SymbolToken "if"
OpenParenToken
SymbolToken "="
SymbolToken "n"
NumberToken 0
CloseParenToken
NumberToken 1
OpenParenToken
SymbolToken "*"
SymbolToken "n"
OpenParenToken
SymbolToken "!"
OpenParenToken
SymbolToken "-"
SymbolToken "n"
NumberToken 1
CloseParenToken
CloseParenToken
CloseParenToken
CloseParenToken
CloseParenToken
CloseParenToken
EndToken

*)
