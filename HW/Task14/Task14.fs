// 40.1
let rec sum = function
    | (_, []) -> 0
    | (p, head :: tail) when p(head) -> head + sum (p, tail)
    | (p, head :: tail) -> sum (p, tail)

// 40.2.1
let rec count (xs, n) =
    let rec find_numbers = function
        | ([], _) -> []
        | head :: _ as current_list, number when head > number -> current_list
        | head :: _ as current_list, number when head = number -> current_list
        | _ :: tail, number -> find_numbers (tail, number)
    
    let rec count_numbers = function
        | ([], _, acc) -> acc
        | (head :: _, number, acc) when head <> number -> acc 
        | (_ :: tail, number, acc) -> count_numbers (tail, number, acc + 1)
    
    let find = find_numbers >> (fun l -> (l, n, 0)) >> count_numbers
    find (xs, n)

// 40.2.2
let rec insert = function
    | ([], n) -> [n]
    | (head :: tail, n) when head >= n -> n :: head :: tail
    | (head :: tail, n) -> head :: insert (tail, n)

// 40.2.3
let rec intersect  = function
    | ([], _) | (_, []) -> []
    | (head_xs1 :: tail_xs1, (head_xs2 :: _ as xs2)) when head_xs1 < head_xs2 -> intersect (tail_xs1, xs2)
    | (head_xs1 :: _ as xs1, head_xs2 :: tail_xs2) when head_xs1 > head_xs2 -> intersect (xs1, tail_xs2)
    | (head_xs1 :: tail_xs1, _ :: tail_xs2) -> head_xs1 :: intersect (tail_xs1, tail_xs2)
    
// 40.2.4
let rec plus = function
        | ([], xs2) -> xs2
        | (xs1, []) -> xs1
        | (head_xs1 :: tail_xs1, (head_xs2 :: _ as xs2)) when head_xs1 < head_xs2 -> head_xs1 :: plus (tail_xs1, xs2)
        | (head_xs1 :: _ as xs1, head_xs2 :: tail_xs2) when head_xs1 > head_xs2 -> head_xs2 :: plus (xs1, tail_xs2)
        | (head_xs1 :: tail_xs1, head_xs2 :: tail_xs2) -> head_xs1 :: head_xs2 :: plus (tail_xs1, tail_xs2)

// 40.2.5
let rec minus  = function
        | (xs1, []) -> xs1
        | ([], _) -> []
        | (head_xs1 :: tail_xs1, (head_xs2 :: _ as xs2)) when head_xs1 < head_xs2 -> head_xs1 :: minus (tail_xs1, xs2)
        | (head_xs1 :: _ as xs1, head_xs2 :: tail_xs2) when head_xs1 > head_xs2 -> minus (xs1, tail_xs2)
        | (_ :: tail_xs1, _ :: tail_xs2) -> minus (tail_xs1, tail_xs2)

// 40.3.1
let rec smallest lst =
    let rec find_smallest = function
        | ([], current_smallest) when current_smallest = None -> None
        | ([], current_smallest) -> current_smallest
        | (head :: tail, current_smallest) ->
            let new_smallest =
                if current_smallest = None || Option.get current_smallest > head then Some head
                else current_smallest
            find_smallest (tail, new_smallest)
            
    find_smallest (lst, None)

// 40.3.2
let rec delete = function
        | (_, []) -> []
        | (n, head :: tail) when head = n -> tail
        | (n, head :: tail) -> head :: delete (n, tail)

// 40.3.3
let rec sort = function
    | [] -> []
    | [x] -> [x]
    | lst ->
        let smallest_number = Option.get (smallest lst)
        let new_lst = delete (smallest_number, lst)
        smallest_number :: sort new_lst

// 40.4
let rec revrev lst =
    let rec rev = function
        | [] -> []
        | head :: tail -> rev tail @ [head]
        
    let rec map f l =
        match l with
            | [] -> []
            | head :: tail -> f head :: map f tail
        
    lst |> map rev |> rev