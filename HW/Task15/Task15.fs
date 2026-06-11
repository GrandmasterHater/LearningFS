// 41.4.1
let list_filter f xs = List.foldBack (fun i acc -> if f(i) then i :: acc else acc) xs []

// 41.4.2
let sum (p, xs) = List.fold (fun acc i -> if p(i) then i + acc else acc) 0 xs

// 41.4.3
let revrev lst =
    let rev l = List.fold (fun acc i -> i :: acc) [] l
    List.fold (fun acc i -> rev i :: acc) [] lst

