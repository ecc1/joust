open Parser

let to_string = function
  | IDENTIFIER id -> "Identifier " ^ Syntax.id_string id
  | PRIMITIVE_TYPE s -> "PrimitiveType " ^ s
  | LITERAL s -> "Literal " ^ s

  (* 3.11 Separators *)
  | LP -> "("
  | RP -> ")"
  | LC -> "{"
  | RC -> "}"
  | LB -> "["
  | RB -> "]"
  | SM -> ";"
  | CM -> ","
  | DOT -> "."

  (* 3.12 Operators *)
  | EQ -> "="
  | GT -> ">"
  | LT -> "<"
  | NOT -> "!"
  | COMPL -> "~"
  | COND -> "?"
  | COLON -> ":"
  | EQ_EQ -> "=="
  | LE -> "<="
  | GE -> ">="
  | NOT_EQ -> "!="
  | AND_AND -> "&&"
  | OR_OR -> "||"
  | INCR -> "++"
  | DECR -> "--"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIV -> "/"
  | AND -> "&"
  | OR -> "|"
  | XOR -> "^"
  | MOD -> "%"
  | LS -> "<<"
  | SRS -> ">>"
  | URS -> ">>>"
  | OPERATOR_EQ op -> op

  | ABSTRACT -> "abstract"
  | BOOLEAN -> "boolean"
  | BREAK -> "break"
  | BYTE -> "byte"
  | CASE -> "case"
  | CATCH -> "catch"
  | CHAR -> "char"
  | CLASS -> "class"
  | CONST -> "const"
  | CONTINUE -> "continue"
  | DEFAULT -> "default"
  | DO -> "do"
  | DOUBLE -> "double"
  | ELSE -> "else"
  | EXTENDS -> "extends"
  | FINAL -> "final"
  | FINALLY -> "finally"
  | FLOAT -> "float"
  | FOR -> "for"
  | GOTO -> "goto"
  | IF -> "if"
  | IMPLEMENTS -> "implements"
  | IMPORT -> "import"
  | INSTANCEOF -> "instanceof"
  | INT -> "int"
  | INTERFACE -> "interface"
  | LONG -> "long"
  | NATIVE -> "native"
  | NEW -> "new"
  | PACKAGE -> "package"
  | PRIVATE -> "private"
  | PROTECTED -> "protected"
  | PUBLIC -> "public"
  | RETURN -> "return"
  | SHORT -> "short"
  | STATIC -> "static"
  | STRICTFP -> "strictfp"
  | SUPER -> "super"
  | SWITCH -> "switch"
  | SYNCHRONIZED -> "synchronized"
  | THIS -> "this"
  | THROW -> "throw"
  | THROWS -> "throws"
  | TRANSIENT -> "transient"
  | TRY -> "try"
  | VOID -> "void"
  | VOLATILE -> "volatile"
  | WHILE -> "while"

  | EOF -> "EOF"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let result = Lexer.token lexbuf in
    print_endline (to_string result);
    if result = EOF then
      exit 0
  done
