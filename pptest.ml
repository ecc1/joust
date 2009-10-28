open Lexing

let usage () =
  Printf.eprintf "Usage: %s file ...\n" Sys.argv.(0);
  exit 1

let errors = ref 0

let error msg =
  incr errors;
  Printf.eprintf "%s: %s\n" (Lexer.location ()) msg

let parse () =
  try
    let result = Parser.goal Lexer.token (Lexer.open_file ()) in
    Parsing.clear_parser ();
    Pretty.print Format.std_formatter result
  with e -> error (Printexc.to_string e)

let _ =
  let argc = Array.length Sys.argv in
  if argc <= 1 then
    usage ();
  for i = 1 to argc-1 do
    Lexer.set_file_name Sys.argv.(i);
    parse ()
  done;
  exit !errors
    
