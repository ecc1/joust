val set_file_name: string -> unit

val with_lexbuf: (Lexing.lexbuf -> 'a) -> 'a

val next_line: Lexing.lexbuf -> unit

val location: unit -> string

val begin_comment: Lexing.lexbuf -> unit
val continue_comment: Lexing.lexbuf -> unit
val end_comment: Lexing.lexbuf -> unit

val eol_comment: Lexing.lexbuf -> unit

val lexeme_pos: Lexing.lexbuf -> int

type comment = { mutable buffer: string; mutable pos: int } 

type comments = comment list

val comments: unit -> comment list
