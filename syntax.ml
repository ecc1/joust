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

let compilation_unit (pkg, ims, dcls) =	
  { package = pkg; imports = ims; decls = dcls }

let class_decl (mods, name, super, ifs, body) =
  { cl_mods = mods; cl_name = name; cl_super = super;
    cl_impls = ifs; cl_body = body }

let method_decl (hdr, body) =
  { hdr with m_body = body }

let interface_decl (mods, name, extends, body) =
  { if_mods = mods; if_name = name; if_exts = extends; if_body = body }

(* Move array dimensions from variable name to type. *)

let rec canon_var mods t v =
  match v with
  | IdentDecl str -> { v_mods = mods; v_type = t; v_name = str }
  | ArrayDecl v' -> canon_var mods (ArrayType t) v'

let method_header (mods, mtype, (v, formals), throws) =
  { m_var = canon_var mods mtype v; m_formals = formals;
    m_throws = throws; m_body = Empty }

(* Return a list of field declarations in canonical form. *)

let decls f (mods, vtype, vars) =
  let dcl (v, init) =
    f { f_var = canon_var mods vtype v; f_init = init }
  in
  List.map dcl vars

let field_decls = decls (fun x -> Field x)

let var_decls = decls (fun x -> LocalVar x)

let formal_decl (mods, t, v) = canon_var mods t v

let constructor (mods, (id, formals), throws, body) =
  Constructor { m_var = { v_mods = mods; v_type = TypeName []; v_name = id};
		m_formals = formals; m_throws = throws; m_body = body }

(* Convert an expression, which must be a name, into a named type. *)

let type_name exp =
  match exp with
  | Name name -> TypeName name
  | _ -> raise Parsing.Parse_error
