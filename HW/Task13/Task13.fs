// 39.1
let rec rmodd = function
    | [] -> []
    | [x] -> []
    | head :: tail :: rest_list -> tail :: rmodd rest_list

// 39.2
let rec del_even = function
    | [] -> []
    | head :: tail when head % 2 = 0 -> del_even tail
    | head :: tail -> head :: del_even tail 

// 39.3
let rec multiplicity x = function
    | [] -> 0
    | head :: tail when head = x -> 1 + multiplicity x tail
    | head :: tail -> multiplicity x tail

// 39.4
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | first :: second :: rest_list ->
        let (even_list, odd_list) = split rest_list
        (first :: even_list, second :: odd_list)

exception DifferentListLengths 

// 39.5
let rec zip  = function
    | ([], []) -> []
    | (xs1, [])  -> raise DifferentListLengths
    | ([], xs2) -> raise DifferentListLengths
    | (first_head :: first_tail, second_head :: second_tail) -> (first_head, second_head) :: zip (first_tail, second_tail)