// 50.2.1
let fac_seq = seq {
        let mutable fact = 1
        
        yield fact
        
        let mutable i = 1
        
        while true do
            fact <- fact * i
            yield fact
            i <- i + 1
    }

// 50.2.2
let seq_seq = seq {
        yield 0
        
        let mutable i = 1
        
        while true do
            yield -i
            yield i
            i <- i + 1
    }