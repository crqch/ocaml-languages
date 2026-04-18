def hd x = match x with (hd, _) -> hd

def tl x = match x with (_, tl) -> tl

def nil = ()

def is_nil = fun x -> x == ()

def rec len xs =
  if xs == () then 0
  else match xs with (h, t) -> 1 + len t

def abs x =
    if x < 0 then 0-x else x

def split xs =
  if xs == () then ((), ())
  else
    let d = funrec d xxs ys n k ->
      if abs (n-k) <= 1 then (xxs, ys)
      else d (hd ys,xxs) (tl ys) (n+1) (k-1)
    in d (hd xs,()) (tl xs) 1 (len xs)

def rec merge xs ys =
  if xs == () then ys
  else if ys == () then xs
  else if hd xs > hd ys then (hd ys, merge (tl ys) xs)
  else (hd xs,merge (tl xs) ys)

def rec merge_sort xs =
    if xs == () then ()
    else if tl xs == () then xs
    else match (split xs) with (left,right) -> merge (merge_sort left) (merge_sort right)
