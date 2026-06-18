// 49.5.1
let even_seq = Seq.initInfinite (fun i -> i * 2)

// 49.5.2
let fac_seq =
    let rec fact f n =
        match n with 
            | n when n <= 0 -> f 1
            | n -> fact (fun x -> f (n * x)) (n - 1)
        
    Seq.initInfinite (fact id)
        

// 49.5.3
let seq_seq = Seq.initInfinite (fun i -> if i = 0 then Seq.singleton 0 else Seq.ofList [-i; i]) |> Seq.concat 

