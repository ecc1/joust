open Lexing

let usage () =
  Printf.eprintf "Usage: %s file ...\n" Sys.argv.(0);
  exit 1

let error msg =
  Printf.eprintf "%s: %s\n" (Source.location ()) msg

let parse () =
  try
    Source.with_lexbuf
      (fun lexbuf ->
	ignore (Parser.goal Lexer.token lexbuf);
	Printf.eprintf "%s: OK\n" (Source.location ()))
  with e -> error (Printexc.to_string e)

let _ =
  let argc = Array.length Sys.argv in
  if argc <= 1 then
    usage ();
  for i = 1 to argc-1 do
    Source.set_file_name Sys.argv.(i);
    parse ()
  done
