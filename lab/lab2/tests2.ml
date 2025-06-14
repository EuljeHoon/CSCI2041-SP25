(*
  CSci 2041 Tests for Lab Assignment 2

    James Moen
    04 Feb 25

  These tests are worth 40 points total.
*)

(* RAT PRINT. Print a pair (N, D) as the fraction N / D. You don't have to know
   how this works. *)
   let rec gcd i j =
    if i <> 0
      then if j > i
        then gcd i (j - i)
        else gcd (i- j) j
    else j;;
  
  (*rat function*)
  let rat n d =
    let newGCD = gcd n d in
    (n / newGCD, d / newGCD)
  ;;
  
  (*ratAdd function*)
  let ratAdd a b =
    let (n1, d1) = a in
    let (n2, d2) = b in
    rat((n1 * d2) + (n2 * d1))(d1 * d2)
  ;;
  
  (*ratMul function*)
  let ratMul a b =
    let (n1, d1) = a in
    let (n2, d2) = b in
    rat(n1 * n2)(d1 * d2)
  ;;
  
  (*ratDiv function*)
  let ratDiv a b =
    let (n1, d1) = a in
    let (n2, d2) = b in
    rat(n1 * d2)(d1 * n2)
  ;;
  
  (*ratGt function*)
  let ratGt a b =
    let (n1, d1) = a in
    let (n2, d2) = b in
    (n1 * d2) > (n2 * d1)
  ;;
  
  (*euler function*)
  let euler() =
  let epsilon = (1, 100000) in
  let rec e c s t =
    if ratGt t epsilon then 
      let s' = ratAdd s t in
      let t' = ratDiv t (c, 1) in
      e (c + 1) s' t'
    else
      s
  in e 1 (0, 1) (1, 1)
  ;;
let ratPrint (n, d) =
  Printf.printf "%i / %i\n" n d ;;

(* BOOL PRINT. Print a BOOL B. You don't have to know how this works either. *)

let boolPrint b =
  Printf.printf "%b\n" b ;;

(* Test the rational arithmetic functions. *)

ratPrint (rat 1 2) ;;                                       (* 2 pts: 1 / 2 *)

ratPrint (rat 10 20) ;;                                     (* 2 pts: 1 / 2 *)

ratPrint (ratAdd (rat 1 2) (rat 1 2)) ;;                    (* 2 pts: 1 / 1 *)

ratPrint (ratAdd (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: 5 / 6 *)

ratPrint (ratMul (rat 1 2) (rat 10 1)) ;;                   (* 2 pts: 5 / 1 *)

ratPrint (ratMul (rat 2 3) (rat 4 5)) ;;                    (* 2 pts: 8 / 15 *)

ratPrint (ratDiv (rat 1 2) (rat 10 2)) ;;                   (* 2 pts: 1 / 10 *)

ratPrint (ratDiv (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: 3 / 2 *)

boolPrint (ratGt (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: true *)

boolPrint (ratGt (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: false *)

(* The big finish. Compute E. *)

ratPrint (euler ()) ;;                             (* 20 pts: 109601 / 40320 *)
