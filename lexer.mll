(* ocamllex lexer for Java *)

(*
	The Java Language Specification

	Second Edition

	James Gosling
	Bill Joy
	Guy Steele
	Gilad Bracha
*)

{

open Parser

let identifier buf =
  let s = Lexing.lexeme buf in
  match Reserved.lookup s with
  | Some t -> t
  | None -> IDENTIFIER s

let literal buf =
  LITERAL (Lexing.lexeme buf)

} 

(* CHAPTER 3: Lexical Structure *)

(* 3.4 Line Terminators *)

let LF = '\n'    (* newline *)
let CR = '\r'    (* return *)

let LineTerminator = LF | CR | CR LF

(* 3.5 Input Elements and Tokens *)

let SUB = '\026' (* control-Z *) (* decimal *)

(* 3.6 White Space *)

let SP = ' '     (* space *)
let HT = '\t'    (* horizontal tab *)
let FF = '\009'  (* form feed *) (* decimal *)

let WhiteSpace = SP | HT | FF | LineTerminator

(* 3.7 Comments *)

let TraditionalComment = "/*" ([^ '*'] | '*' [^ '/'])* "*/"
let EndOfLineComment = "//" [^ '\n' '\r']* LineTerminator
let Comment = TraditionalComment | EndOfLineComment

(* 3.8 Identifiers *)

let Letter = ['A'-'Z' 'a'-'z' '_' '$']
let Digit = ['0'-'9']
let Identifier = Letter (Letter | Digit)*

(* 3.10.1 Integer Literals *)

let IntegerTypeSuffix = ['l' 'L']

let DecimalIntegerLiteral = ('0' | ['1'-'9'] Digit*) IntegerTypeSuffix?

let HexDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let HexIntegerLiteral = '0' ['x' 'X'] HexDigit+ IntegerTypeSuffix?

let OctalDigit = ['0'-'7']
let OctalIntegerLiteral = '0' OctalDigit+ IntegerTypeSuffix?

let IntegerLiteral =
  DecimalIntegerLiteral
| HexIntegerLiteral
| OctalIntegerLiteral

(* 3.10.2 Floating-Point Literals *)

let ExponentPart = ['e' 'E'] ['+' '-']? Digit+

let FloatTypeSuffix = ['f' 'F' 'd' 'D']

let FloatingPointLiteral =
  (Digit+ '.' Digit* | '.' Digit+) ExponentPart? FloatTypeSuffix?
| Digit+ (ExponentPart FloatTypeSuffix? | ExponentPart? FloatTypeSuffix)

(* 3.10.3 Boolean Literals *)

let BooleanLiteral = "true" | "false"

(* 3.10.6 Escape Sequences for Character and String Literals *)

let EscapeSequence =
  '\\' ['b' 't' 'n' 'f' 'r' '"' '\'' '\\']
| '\\' (['0'-'3']? OctalDigit)? OctalDigit

(* 3.10.4 Character Literals *)

let SingleCharacter = [^ '\'' '\\' '\n' '\r']
let CharacterLiteral = '\'' (SingleCharacter | EscapeSequence) '\''

(* 3.10.5 String Literals *)

let StringCharacter = [^ '"' '\\' '\n' '\r']
let StringLiteral = '"' (StringCharacter | EscapeSequence)* '"'

(* 3.10.7 The Null Literal *)

let NullLiteral = "null"

(* 3.10 Literals *)

let Literal =
  IntegerLiteral
| FloatingPointLiteral
| BooleanLiteral
| CharacterLiteral
| StringLiteral
| NullLiteral

rule token = parse
| WhiteSpace  { token lexbuf }
| Comment  { token lexbuf }
| Identifier  { identifier lexbuf }
| Literal  { literal lexbuf }

(* 3.11 Separators *)
| '('  { LP }
| ')'  { RP }
| '{'  { LC }
| '}'  { RC }
| '['  { LB }
| ']'  { RB }
| ';'  { SM }
| ','  { CM }
| '.'  { DOT }

(* 3.12 Operators *)
| "="  { EQ }
| ">"  { GT }
| "<"  { LT }
| "!"  { NOT }
| "~"  { COMPL }
| "?"  { COND }
| ":"  { COLON }
| "=="  { EQ_EQ }
| "<="  { LE }
| ">="  { GE }
| "!="  { NOT_EQ }
| "&&"  { AND_AND }
| "||"  { OR_OR }
| "++"  { INCR }
| "--"  { DECR }
| "+"  { PLUS }
| "-"  { MINUS }
| "*"  { TIMES }
| "/"  { DIV }
| "&"  { AND }
| "|"  { OR }
| "^"  { XOR }
| "%"  { MOD }
| "<<"  { LS }
| ">>"  { SRS }
| ">>>"  { URS }
| "+="  { OPERATOR_EQ PLUS }
| "-="  { OPERATOR_EQ MINUS }
| "*="  { OPERATOR_EQ TIMES }
| "/="  { OPERATOR_EQ DIV }
| "&="  { OPERATOR_EQ AND }
| "|="  { OPERATOR_EQ OR }
| "^="  { OPERATOR_EQ XOR }
| "%="  { OPERATOR_EQ MOD }
| "<<="  { OPERATOR_EQ LS }
| ">>="  { OPERATOR_EQ SRS }
| ">>>="  { OPERATOR_EQ URS }

| SUB? eof { EOF }
