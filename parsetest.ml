open Lexing

let parse () =
  let buf = Lexing.from_channel stdin in
  let result = Parser.goal Lexer.token buf in
  exit 0

let _ =
  Printexc.catch parse ()
