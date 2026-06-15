// 47.4.1
let f n =
    let mutable result = 1
    let fact x = result <- result * x
    List.iter fact [1..n]
    result
    
// 47.4.2
let fibo n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> let nums = [|0; 1|]
           let i = ref 2
           while !i <= n do
               let new_res = nums.[0] + nums.[1]
               nums.[0] <- nums.[1] ; nums.[1] <- new_res
               i := !i + 1
        
           nums.[1]
           
           