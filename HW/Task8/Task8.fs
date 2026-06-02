let curry (f : int * int -> int)  = fun x -> fun y -> f (x, y)

let uncurry (f : int -> int -> int) = fun (x, y) -> f x y
