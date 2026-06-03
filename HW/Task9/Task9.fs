
let to_copper (gold, silver, copper) = gold * 20 * 12 + silver * 12 + copper
let to_money c =
    let silver = c / 12
    (silver / 20, silver % 20, c % 12)

// 23.4.1
let (.+.) x y = to_copper x + to_copper y |> to_money
let (.-.) x y = to_copper x - to_copper y |> to_money

// 23.4.2
let (.+) x y =
    let a, b = x
    let c, d = y
    (a + c, b + d)
let (.-) x y =
    let inv (x, y) = (-x, -y)
    x .+ inv(y)
let (.*) x y =
    let a, b = x
    let c, d = y
    (a * c - b * d, b * c + a * d)
let (./) x y = 
    let c, d = y
    let pow_sum = c * c + d * d
    x .* (c / pow_sum, -d / pow_sum)


