type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y =
    let to_minutes { hours = h; minutes = m; f = fd } =
        let calc_by_day_part = function
            | AM -> h * 60 + m
            | PM -> (h + 12) * 60 + m
            
        calc_by_day_part fd
    
    to_minutes x > to_minutes y

