open Format
open Syntax

(* Print a generic separated list.
   [pf] is a formatter for elements of the list.
   [sep] is a function to print the separator between elements. *)

let rec print_sep_list pf sep f list =
  match list with
  | [] -> ()
  | [x] -> pf f x
  | x :: rest -> fprintf f "%a%t%a" pf x sep (print_sep_list pf sep) rest

(* Print a generic option value.
   [pf] is a formatter for the value, if present.
   Prints a space before the value when present. *)

let print_option pf f opt =
  match opt with
  | Some x -> fprintf f " %a" pf x
  | None -> ()

(* Print a list of items with no additional separators.
   [pf] is a formatter for elements of the list. *)

let print_list pf =
  print_sep_list pf (fun f -> ())

(* Print a comma-separated list.
   [pf] is a formatter for elements of the list. *)

let print_comma_list pf =
  print_sep_list pf (fun f -> fprintf f ", ")

(* Print a space-separated list.
   [pf] is a formatter for elements of the list. *)

let print_space_list pf =
  print_sep_list pf (fun f -> fprintf f " ")

(* Print a newline-separated list.
   [pf] is a formatter for elements of the list. *)

let print_newline_list pf =
  print_sep_list pf (fun f -> fprintf f "@\n")

let print_name =
  print_sep_list pp_print_string (fun f -> fprintf f ".")

let rec print_type f t =
  match t with
  | TypeName n -> print_name f n
  | ArrayType t' -> fprintf f "%a[]" print_type t'

let modifier_to_string m =
  match m with
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"
  | Abstract -> "abstract"
  | Static -> "static"
  | Final -> "final"
  | StrictFP -> "strictfp"
  | Transient -> "transient"
  | Volatile -> "volatile"
  | Synchronized -> "synchronized"
  | Native -> "native"

(* Operator precedence.
   Used by print_prec to eliminate unnecessary parentheses. *)

type precedence = Left of int | Right of int

let operator_precedence e =
  match e with
  | Literal _
  | ClassLiteral _
  | NewClass _
  | NewQualifiedClass _
  | NewArray _
  | Dot _
  | Call _
  | ArrayAccess _
  | Name _
    -> Left 16

  | Postfix _
    -> Left 15

  | Prefix ("++", _)
  | Prefix ("--", _)
  | Prefix ("+", _)
  | Prefix ("-", _)
    -> Right 14

  | Prefix ("~", _)
  | Prefix ("!", _)
  | Cast _
    -> Right 13

  | Infix (_, "*", _)
  | Infix (_, "/", _)
  | Infix (_, "%", _)
    -> Left 12

  | Infix (_, "+", _)
  | Infix (_, "-", _)
    -> Left 11

  | Infix (_, "<<", _)
  | Infix (_, ">>", _)
  | Infix (_, ">>>", _)
    -> Left 10

  | Infix (_, "<", _)
  | Infix (_, ">", _)
  | Infix (_, "<=", _)
  | Infix (_, ">=", _)
  | InstanceOf _
    -> Left 9

  | Infix (_, "==", _)
  | Infix (_, "!=", _)
    -> Left 8

  | Infix (_, "&", _) -> Left 7
  | Infix (_, "^", _) -> Left 6
  | Infix (_, "|", _) -> Left 5
  | Infix (_, "&&", _) -> Left 4
  | Infix (_, "||", _) -> Left 3

  | Conditional _ -> Left 2

  | Assignment _ -> Right 1

  | _ -> raise (Invalid_argument "precedence")

let precedence e =
  match operator_precedence e with
  | Left n -> (2*n, 2*n+1)
  | Right n -> (2*n+1, 2*n)

let rec print f comp =
  pp_set_margin f 0;
  pp_open_box f 0;
  begin
    match comp.package with
    | Some pkg -> fprintf f "package %a;@\n@\n" print_name pkg
    | None -> ()
  end;
  if comp.imports <> [] then
    begin
      print_newline_list (fun f -> fprintf f "import %a;" print_name)
	f comp.imports;
      fprintf f "@\n@\n";
    end;
  print_decl_list f comp.decls;
  pp_print_newline f ()

and print_decl_list f =
  print_sep_list print_decl (fun f -> fprintf f "@\n@\n") f

and print_decl f d =
  match d with
  | Class c -> print_class f c
  | Interface i -> print_interface f i
  | Field fld -> fprintf f "%a;" print_field fld
  | Method m -> print_method f m
  | InstanceInit st -> fprintf f "%a" print_stmt st
  | StaticInit st -> fprintf f "static %a" print_stmt st
  | Constructor m -> print_method f m

and print_class f c =
  fprintf f "%aclass %s%a"
    print_modifiers c.cl_mods c.cl_name
    (print_option (fun f -> fprintf f "extends %a" print_name)) c.cl_super;
  if c.cl_impls <> [] then
    fprintf f " implements %a" (print_comma_list print_name) c.cl_impls;
  fprintf f " %a" print_class_body c.cl_body

and print_modifiers f mods =
  print_space_list (fun f m -> fprintf f "%s" (modifier_to_string m)) f mods;
  if mods <> [] then
    fprintf f " "

and print_class_body f body =
  fprintf f "{@\n@[<2>  %a@]@\n}" print_decl_list body

and print_interface f i =
  fprintf f "%ainterface %s" print_modifiers i.if_mods i.if_name;
  if i.if_exts <> [] then
    fprintf f " extends %a" (print_comma_list print_name) i.if_exts;
  fprintf f " %a" print_class_body i.if_body

and print_field f fld =
  fprintf f "%a%a" print_var fld.f_var
    (print_option (fun f -> fprintf f "= %a" print_init)) fld.f_init

and print_init f init =
  match init with
  | ExprInit e -> print_expr f e
  | ArrayInit inits ->
      fprintf f "{ %a }" print_init_list inits

and print_prec prec f e =
  let (left, right) = precedence e in
  if prec > min left right then
    fprintf f "(%a)" print_expr e
  else
    match e with
    | Literal s ->
	fprintf f "%s" s
    | ClassLiteral t ->
	fprintf f "%a.class" print_type t
    | NewClass (t, args, opt) ->
	fprintf f "new %a(%a)%a"
	  print_type t print_expr_list args (print_option print_class_body) opt
    | NewQualifiedClass (e, s, args, opt) ->
	fprintf f "%a.new %s(%a)%a"
	  (print_prec left) e s print_expr_list args
	  (print_option print_class_body) opt
    | NewArray (t, dims, n, opt) ->
	fprintf f "new %a%a%a%a"
	  print_type t print_dimensions dims print_extra_dimensions n
	  (print_option print_init) opt
    | Dot (e, s) ->
	fprintf f "%a.%s" (print_prec left) e s
    | Call (e, args) ->
	fprintf f "%a(%a)" (print_prec left) e print_expr_list args
    | ArrayAccess (e1, e2) ->
	fprintf f "%a[%a]" (print_prec left) e1 print_expr e2
    | Postfix (e, op) ->
	fprintf f "%a%s" (print_prec left) e op
    | Prefix (op, e) ->
	fprintf f "%s%a" op (print_prec right) e
    | Cast (t, e) ->
	fprintf f "(%a)%a" print_type t (print_prec right) e
    | Infix (e1, op, e2) ->
	fprintf f "%a %s %a" (print_prec left) e1 op (print_prec right) e2
    | InstanceOf (e, t) ->
	fprintf f "%a instanceof %a" (print_prec left) e print_type t
    | Conditional (e1, e2, e3) ->
	fprintf f "%a ? %a : %a"
	  (print_prec left) e1 (print_prec left) e2 (print_prec right) e3
    | Assignment (e1, op, e2) ->
	fprintf f "%a %s %a" (print_prec left) e1 op (print_prec right) e2
    | Name n ->
	print_name f n

and print_expr f = print_prec 0 f

and print_expr_list f =
  print_comma_list print_expr f

and print_dimensions f =
  print_list (fun f -> fprintf f "[%a]" print_expr) f

and print_extra_dimensions f n =
  if n > 0 then
    fprintf f "[]%a" print_extra_dimensions (n-1)

and print_init_list f =
  print_comma_list print_init f

and print_method f m =
  fprintf f "%a(%a)"
    print_var m.m_var (print_comma_list print_var) m.m_formals;
  if m.m_throws <> [] then
    fprintf f " throws %a" (print_comma_list print_name) m.m_throws;
  fprintf f " %a" print_stmt m.m_body

and print_var f v =
  if v.v_type = TypeName [] then
    (* special case for constructor: variable with empty type name *)
    fprintf f "%a%s" print_modifiers v.v_mods v.v_name
  else
    fprintf f "%a%a %s" print_modifiers v.v_mods print_type v.v_type v.v_name

and print_stmt f stmt =
  match stmt with
  | Block [] ->
      fprintf f "{@\n}"
  | Block list ->
      fprintf f "{@\n@[<2>  %a@]@\n}" print_stmt_list list
  | LocalVar fld ->
      fprintf f "%a;" print_field fld
  | LocalClass c ->
      print_class f c
  | Empty ->
      fprintf f ";"
  | Label (lab, st) ->
      fprintf f "%s: %a" lab print_stmt st
  | Expr e ->
      fprintf f "%a;" print_expr e
  | If (e, s1, None) ->
      fprintf f "if (%a)%a" print_expr e print_body s1
  | If (e, s1, Some s2) ->
      begin
	fprintf f "if (%a)%a" print_expr e print_body s1;
	(match s1 with
	| Block (_::_) -> fprintf f " "
	| _ -> fprintf f "@\n");
	fprintf f "else";
	(match s2 with
	| If _ -> fprintf f " %a" print_stmt s2
	| _ -> print_body f s2)
      end
  | Switch (e, sw) ->
      fprintf f "switch (%a) %a" print_expr e print_switch sw
  | While (e, st) ->
      fprintf f "while (%a)%a" print_expr e print_body st
  | Do (st, e) ->
      fprintf f "do%a while (%a);" print_body st print_expr e
  | For (init, test, update, st) ->
      fprintf f "for (%a;%a;%t%a)%a"
	print_for_clause init
	(print_option print_expr) test
	(fun f -> if update <> [] then fprintf f " ")
	print_for_clause update
	print_body st
  | Break opt ->
      fprintf f "break%a;" (print_option pp_print_string) opt
  | Continue opt ->
      fprintf f "continue%a;" (print_option pp_print_string) opt
  | Return opt ->
      fprintf f "return%a;" (print_option print_expr) opt
  | Throw e ->
      fprintf f "throw %a;" print_expr e
  | Sync (e, st) ->
      fprintf f "synchronized (%a)%a" print_expr e print_body st
  | Try (st, catches, finally) ->
      begin
	fprintf f "try %a" print_stmt st;
	if catches <> [] then
	  fprintf f " %a" print_catches catches;
	print_option (fun f -> fprintf f "finally %a" print_stmt)
	  f finally;
      end

and print_stmt_list f =
  print_newline_list print_stmt f

and print_body f st =
  match st with
  | Block (_::_) -> fprintf f " %a" print_stmt st
  | _ -> fprintf f "@\n@[<2>  %a@]" print_stmt st

and print_switch f sw =
  fprintf f "{@\n@[<2>  %a@]@\n}" (print_newline_list print_sw) sw

and print_sw f (cases, stmts) =
  fprintf f "%a@\n@[<2>  %a@]"
    (print_newline_list print_case) cases print_stmt_list stmts

and print_case f c =
  match c with
  | Case e -> fprintf f "case %a:" print_expr e
  | Default -> fprintf f "default:"

and print_for_clause f list =
  match list with
  | Expr _ :: _ -> print_comma_list print_expr_stmt f list
  | [LocalVar fld] -> print_field f fld
  | LocalVar fld :: rest ->
      fprintf f "%a, %a" print_field fld
	(print_comma_list (print_for_local_var fld.f_var.v_type)) rest
  | [] -> ()
  | _ -> raise (Invalid_argument "print_for_clause")

and print_expr_stmt f stmt =
  match stmt with
  | Expr e -> print_expr f e
  | _ -> raise (Invalid_argument "print_expr_stmt")

and print_for_local_var t f st =
  let rec convert typ s =
    if typ = t then s
    else match typ with
    | ArrayType typ' -> convert typ' (s ^ "[]")
    | TypeName _ -> raise (Failure "print_for_local_var: convert")
  in
  match st with
  | LocalVar fld ->
      fprintf f "%s%a"
	(convert fld.f_var.v_type fld.f_var.v_name)
	(print_option (fun f -> fprintf f "= %a" print_init)) fld.f_init
  | _ -> raise (Failure "print_for_local_var")

and print_catches f  =
  print_space_list print_catch f

and print_catch f (v, st) =
  fprintf f "catch (%a) %a" print_var v print_stmt st
