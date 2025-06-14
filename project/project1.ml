(* POLY. The type of a univariate polynomial with integer coefficients and
   nonnegative integer exponents. EMPTY is a polynomial with no TERMs, which
   may be taken to represent 0. A TERM has a coefficient, an exponent, and a
   pointer to the next TERM, or to EMPTY. *)

type poly =
 Empty |
 Term of int * int * poly ;;

(* PRINT POLY. Print POLY in a way that's allegedly easy to read. You need not
   know how this works. *)

let printPoly poly =

(* PRINTING. Do all the work for PRINT POLY. *)

 let rec printing poly =
  match poly
  with Empty ->
        () |

       Term (coef, expo, terms) ->
        Printf.printf " %c %i x^%i"
         (if coef < 0 then '-' else '+') (abs coef) expo ;
         printing terms

(* This is PRINT POLY's body. *)

 in match poly
    with Empty ->
          Printf.printf "0\n" |

         Term (coef, expo, terms) ->
          Printf.printf "%i x^%i" coef expo ;
          printing terms ;
          Printf.printf "\n" ;;

(*

  YOUR CODE GOES HERE!

*)
exception PolyError;;

let rec term coef expo poly =
   if coef = 0 (*if coef is 0, throw exception*)
      then raise PolyError
   else if expo < 0 (*if expo is less then 0, throw exception*)
      then raise PolyError
   else
      match poly 
      with Empty -> (*if poly is empty, make new term*)
         Term (coef, expo, Empty) |
         Term(newCoef, newExpo, rest) ->
            if expo = newExpo (*if same expo is already exists, throw exception*)
               then raise PolyError
            else if expo > newExpo
               then Term (coef, expo, poly)
            else
               Term (newCoef, newExpo, term coef expo rest)
;;

let rec polyMap makeCoef makeExpo poly =
   match poly
   with Empty -> Empty |
        Term (coef, expo, rest) ->
         Term(makeCoef coef, makeExpo expo, polyMap makeCoef makeExpo rest)
;;

let polyMinus poly =
   polyMap(fun coef -> -coef)(fun expo -> expo)poly
;;

let polyTermMultiply leftCoef leftExpo rightPoly =
   polyMap(fun coef -> coef * leftCoef)(fun expo -> expo + leftExpo) rightPoly
;;

let rec polyPolyAdd leftPoly rightPoly =
   match (leftPoly, rightPoly)
   with (Empty, poly) -> (*return the other poly if one of the poly is empty*)
      poly |
   (poly, Empty) -> 
      poly |
   (Term(firstCoef, firstExpo, firstRest), Term(secondCoef, secondExpo, secondRest)) ->
      if firstExpo = secondExpo
         then let addedCoef = firstCoef + secondCoef
            in 
            if addedCoef = 0 (*delete the term if added coef is 0*)
               then polyPolyAdd firstRest secondRest
            else
               Term(addedCoef, firstExpo, polyPolyAdd firstRest secondRest)
      else if firstExpo > secondExpo
         then Term(firstCoef, firstExpo, polyPolyAdd firstRest rightPoly)
      else
         Term(secondCoef, secondExpo, polyPolyAdd leftPoly secondRest)
;;

let polyPolySubtract leftPoly rightPoly =
   polyPolyAdd leftPoly (polyMinus rightPoly)
;;

let rec polyPolyMultiply leftPoly rightPoly = 
   match leftPoly
   with Empty -> 
      Empty |
      Term(coef, expo, rest) ->
         polyPolyAdd(polyTermMultiply coef expo rightPoly)(polyPolyMultiply rest rightPoly)
;;



(* Examples, for debugging only. THEY DO NOT DETERMINE YOUR GRADE! The comments
   show what should be printed if your functions are correct. *)

(* 3 x^5 + 2 x^4 + 2 x^3 − 1 x^2 + 5 x^0. *)
let p = (term 3 5 (term 2 4 (term 2 3 (term (-1) 2 (term 5 0 Empty))))) ;;
printPoly p ;;

(* 7 x^4 + 1 x^2 − 4 x^1 − 3 x^0. *)
let q = (term 7 4 (term 1 2 (term (-4) 1 (term (-3) 0 Empty)))) ;;
printPoly q ;;

(* 2 x^1 + 3 x^0 *)
let r = (term 2 1 (term 3 0 Empty)) ;;
printPoly r ;;

(* 3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0 *)
printPoly (polyPolyAdd p q) ;;

(* -3 x^5 - 2 x^4 - 2 x^3 + 1 x^2 - 5 x^0 *)
printPoly (polyMinus p) ;;

(* 3 x^5 - 5 x^4 + 2 x^3 - 2 x^2 + 4 x^1 + 8 x^0 *)
printPoly (polyPolySubtract p q) ;;

(* 6 x^6 + 4 x^5 + 4 x^4 - 2 x^3 + 10 x^1 *)
printPoly (polyTermMultiply 2 1 p) ;;

(* 6 x^6 + 13 x^5 + 10 x^4 + 4 x^3 - 3 x^2 + 10 x^1 + 15 x^0 *)
printPoly (polyPolyMultiply p r) ;;