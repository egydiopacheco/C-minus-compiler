open Lexer
open Hashtable

(* This is just an alias of the Hashtable module *)
module H = Hashtable

(* *)
let main () =
  let my_buf = set_filename "stdin" (Lexing.from_channel stdin) in
  let ht = H.create_hash 100 in
  let rec loop = function
    | EOF -> H.to_table EOF ht
    | x   -> (H.to_table x ht); loop (token my_buf)
  in
  loop (token my_buf);
  H.print_hash ht
  let () = main ()

