// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let  prime =
    let rec findPrime = function
        | (i, m) when i * i > m -> true
        | (i, m) when m % i = 0 -> false
        | (i, m) -> findPrime(i + 2, m)
    
    function
    | n when n < 2  -> false
    | n when n < 4 -> true
    | n when n % 2 = 0 -> false
    | n -> findPrime(3, n)
    