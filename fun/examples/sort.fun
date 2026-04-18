def cons = fun x -> fun xs -> (x, xs)

def nil = ()

def head = fun x -> match x with (x,y) -> x

def tail = fun x -> match x with (x,y) -> y

def is_nil = fun x -> x == ()

def insert = funrec insert x -> fun xs ->
  if is_nil xs then
    cons x xs
  else if head xs > x then
    cons x xs
  else
    cons (head xs) (insert x (tail xs))

def sort = funrec sort xs ->
  if is_nil xs then
    xs
  else
    insert (head xs) (sort (tail xs))

def example =
  let xs = cons 4 (cons 7 (cons 2 (cons 6 (cons 3 (cons 1 (cons 5 nil)))))) in
  sort xs
