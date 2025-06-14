open Scanner

exception Can'tParse of string

(*Lisp Values*)
type thing =
  Closure of thing * thing * environment |
  Cons of thing * thing |
  Nil |
  Number of int |
  Primitive of (thing -> environment -> thing) |
  Symbol of string
and
  environment = (string * thing)list

module type Parserish = 
sig
  exception Can'tParse of string  (*Raise Error message when parsing failed*)
  val initialize : string -> unit (*Read from the file*)
  val nextThing : unit -> thing  (*Parse the expression from input*)
end

let token = 
  ref Scanner.EndToken (*Most recent read token*)

let nextToken() =
  token := Scanner.nextToken() (*Store the next input in the token*)

let initialize path =
  Scanner.initialize path; (*Initialize the scanner to read token*)
  nextToken()

let oops message =
  raise (Can'tParse message) (*error message*)

  (*Parser Function*)
let rec nextThing() =
  match !token
  with Scanner.CloseParenToken ->
        oops "Unexpected paren" |  (*Raise error when ')' in a wrong place*)
       Scanner.EndToken ->
        oops "Unexpected end" |    (*Raise error when EOF found*)
       Scanner.NumberToken n ->
        nextToken();
        Number n |   (*Numeric literal*)
       Scanner.OpenParenToken ->
        nextToken();
        nextThings() | (*Start parsing a list*)
       Scanner.SymbolToken "nil" ->
        nextToken();
        Nil |    (*retuen Nil when it is empty list*)
       Scanner.SymbolToken s ->
        nextToken();
        Symbol s  (*Return a single symbol*)
(*Helper function to parse into Cons*)
and nextThings() =
  match !token
  with Scanner.CloseParenToken ->
        nextToken();
        Nil |  (*Return Nil when it is end of the list*)
       Scanner.EndToken ->
        oops "Unexpected end" | (*Raise error when EOF found*)
       _ ->
        let hd = nextThing()
        in
        let tl = nextThings()
        in
        Cons (hd, tl)    (*Building the list with Cons*)

(*Module implementation*)
module Parser : Parserish = 
struct
  exception Can'tParse = Can'tParse
  let initialize = initialize
  let nextThing = nextThing   (*Make exception viewable*)
end