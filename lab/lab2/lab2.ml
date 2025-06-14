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