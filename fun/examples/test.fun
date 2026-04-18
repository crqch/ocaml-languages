
def hd x = match x with (hd, _) -> hd

def tl x = match x with (_, tl) -> tl

def nil = ()

def is_nil = fun x -> x == ()

def rec len xs =
  if xs == () then 0
  else match xs with (h, t) -> 1 + len t
