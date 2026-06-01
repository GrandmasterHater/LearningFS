// 20.3.1
let vat n x =
    if n < 0 || n > 100 then
        -1.0
    else
        x + (x * float(n)) / 100.0

// 20.3.2
let unvat n x =
    if n < 0 || n > 100 then
        -1.0
    else
        x / vat n 1.0

// 20.3.3
let rec min f =
    let rec find_n_rec = function
       | (f, n) when f(n) = 0 -> n
       | (f, n) -> find_n_rec(f, n + 1)
    
    find_n_rec(f, 1)