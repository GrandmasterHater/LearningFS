// 34.1
let rec upto n =
    let rec iterate_from = function
        | (i, n) when n <= 0 -> []
        | (i, n) when i = n -> [n]
        | (i, n) -> i :: iterate_from (i + 1, n)
    
    iterate_from (1, n)

// 34.2
let rec dnto = function
    | n when n <= 0 -> []
    | n -> n :: dnto(n - 1)

// 34.3
let rec evenn n =
    let rec collect_even_numbers = function
        | (i, n) when n < 0 -> []
        | (i, n) when i = n -> [n]
        | (i, n) -> i :: collect_even_numbers (i + 2, n)
        
    let calc_start_value n = (0, n * 2 - 2)
    let get_even = calc_start_value >> collect_even_numbers
    
    get_even n

