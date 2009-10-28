type name = string list

type typ =
  | TypeName of name
  | ArrayType of typ

type modifier =
  | Public
  | Protected
  | Private
  | Abstract
  | Static
  | Final
  | StrictFP
  | Transient
  | Volatile
  | Synchronized
  | Native

type modifiers = modifier list

type var_decl_id =
  | IdentDecl of string
  | ArrayDecl of var_decl_id

type op = string

type compilation_unit =
  { package: name option;
    imports: name list;
    decls: decl list  }

and decl =
  | Class of class_decl
  | Interface of interface
  | Field of field
  | Method of method_decl
  | InstanceInit of stmt
  | StaticInit of stmt
  | Constructor of method_decl

and class_decl =
  { cl_mods: modifiers;
    cl_name: string;
    cl_super: name option;
    cl_impls: name list;
    cl_body: decl list }

and interface =
  { if_mods: modifiers;
    if_name: string;
    if_exts: name list;
    if_body: decl list }

and field =
  { f_var: var;
    f_init: init option }

and method_decl =
  { m_var: var;
    m_formals: var list;
    m_throws: name list;
    m_body: stmt }

and var =
  { v_mods: modifiers;
    v_type: typ;
    v_name: string }

and init =
  | ExprInit of expr
  | ArrayInit of init list

and stmt =
  | Block of stmt list
  | LocalVar of field
  | LocalClass of class_decl
  | Empty
  | Label of string * stmt
  | Expr of expr
  | If of expr * stmt * stmt
  | Switch of expr * (case list * stmt list) list
  | While of expr * stmt
  | Do of stmt * expr
  | For of stmt list * expr option * stmt list * stmt
  | Break of string option
  | Continue of string option
  | Return of expr option
  | Throw of expr
  | Sync of expr * stmt
  | Try of stmt * catch list * stmt option

and case =
  | Case of expr
  | Default

and catch = var * stmt

and expr =
  | Literal of string
  | ClassLiteral of typ
  | NewClass of typ * expr list * decl list option
  | NewQualifiedClass of expr * string * expr list * decl list option
  | NewArray of typ * expr list * int * init option
  | Dot of expr * string
  | Call of expr * expr list
  | Postfix of expr * op
  | Prefix of op * expr
  | Cast of typ * expr
  | Infix of expr * op * expr
  | InstanceOf of expr * typ
  | Conditional of expr * expr * expr
  | Name of string list

val compilation_unit: name option * name list * decl list -> compilation_unit

val class_decl:
    modifiers * string * name option * name list * decl list -> class_decl

val method_decl: method_decl * stmt -> method_decl

val interface_decl: modifiers * string * name list * decl list -> interface

val method_header:
    modifiers * typ * (var_decl_id * var list) * name list -> method_decl

val field_decls:
    modifiers * typ * (var_decl_id * init option) list -> decl list

val var_decls:
    modifiers * typ * (var_decl_id * init option) list -> stmt list

val formal_decl: modifiers * typ * var_decl_id -> var

val constructor: modifiers * (string * var list) * name list * stmt -> decl

val type_name : expr -> typ
