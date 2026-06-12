// 43.3
let try_find key m = if Map.containsKey key m then Some (m.[key]) else None

