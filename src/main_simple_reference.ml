
  
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Adaptive Calender system.\n");
  print_endline "What do you want to do?\n"
  
  (*print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> ()*)

(* Execute the game engine. *)
let () = main ()
