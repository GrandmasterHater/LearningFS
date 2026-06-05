let rec iterate_from_to_step = function
        | (f, t, s) when s = 0 -> []
        | (f, t, s) when f < t && s < 0 -> []
        | (f, t, s) when f > t && s > 0 -> []
        | (f, t, s) when f = t -> [t]
        | (f, t, s) -> f :: iterate_from_to_step (f + s, t, s)

// 34.1
let rec upto n= iterate_from_to_step (1, n, 1)

// 34.2
let rec dnto n = iterate_from_to_step (n, 1, -1)

// 34.3
let rec evenn n =   
    let calc_start_value n = (0, n * 2 - 2, 2)
    let get_even = calc_start_value >> iterate_from_to_step
    
    get_even n

