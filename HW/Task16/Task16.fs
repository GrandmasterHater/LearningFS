// 42.3
let rec allSubsets n k =
    let rec generate_subsets = function
        | _, current_set, acc when Set.count current_set = k -> Set.add current_set acc
        | lst, current_set, acc when List.length lst < (k - Set.count current_set) -> acc
        | [], _, acc -> acc
        | x :: tail, current_set, acc -> let new_acc = generate_subsets(tail, Set.add x current_set, acc)
                                         generate_subsets(tail, current_set, new_acc)
                                 
    generate_subsets ([1 .. n], Set.empty, Set.empty)