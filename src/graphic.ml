open Calendar

let view_event e = 
  ANSITerminal.(print_string [blue] (e.name ^ "\n"));
  ANSITerminal.(print_string [blue] 
                  ((Time.time_to_string e.starts) 
                   ^ ":" ^ (Time.time_to_string e.ends) ^ "\n"));
  ANSITerminal.(print_string [blue] (e.description ^ "\n"))


let view_week c s = ()