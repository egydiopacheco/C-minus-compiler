open Lexer
open Hashtable

(* This is just an alias of the Hashtable module *)
module H = Hashtable

(* This function open a lexbuf and add the output to a hash table *)
let main () =
  let my_buf = set_filename "stdin" (Lexing.from_channel stdin) in
  let ht = H.create_hash 300 in
  let rec loop = function
    | EOF -> H.to_table EOF ht
    | x   -> (H.to_table x ht); loop (token my_buf)
  in
  loop (token my_buf);
  (* Uncomment this line to print the hash table *)
  H.print_hash ht 
  let () = main ()

