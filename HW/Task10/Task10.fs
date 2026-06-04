type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    let to_minutes { hours = h; minutes = m; f = fd } =
        let half_day_minutes = if fd = "AM" then 0 else 12 * 60
        h * 60 + m + half_day_minutes
    
    to_minutes x > to_minutes y
    
    