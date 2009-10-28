/* LALR(1) (ocamlyacc) grammar for Java */

/*
	The Java Language Specification

	Second Edition

	James Gosling
	Bill Joy
	Guy Steele
	Gilad Bracha
*/

%{

open Syntax

%}

%token <string> IDENTIFIER
%token <string> LITERAL
%token <string> PRIMITIVE_TYPE

/*
 * 3.11 Separators
 */
%token LP		/* ( */
%token RP		/* ) */
%token LC		/* { */
%token RC		/* } */
%token LB		/* [ */
%token RB		/* ] */
%token SM		/* ; */
%token CM		/* , */
%token DOT		/* . */

/*
 * 3.12 Operators
 */
%token EQ		/* = */
%token GT		/* > */
%token LT		/* < */
%token NOT		/* ! */
%token COMPL		/* ~ */
%token COND		/* ? */
%token COLON		/* : */
%token EQ_EQ		/* == */
%token LE		/* <= */
%token GE		/* >= */
%token NOT_EQ		/* != */
%token AND_AND		/* && */
%token OR_OR		/* || */
%token INCR		/* ++ */
%token DECR		/* -- */
%token PLUS		/* + */
%token MINUS		/* - */
%token TIMES		/* * */
%token DIV		/* / */
%token AND		/* & */
%token OR		/* | */
%token XOR		/* ^ */
%token MOD		/* % */
%token LS		/* << */
%token SRS		/* >> */
%token URS		/* >>> */
%token <string> OPERATOR_EQ	/* += -= *= /= &= |= ^= %= <<= >>= >>>= */

/*
 * 3.9 Keywords
 */
%token ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE
%token DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO
%token IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG
%token NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN
%token SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE

%token EOF

/*
 * The start production must begin with a lowercase letter,
 * because ocamlyacc defines the parsing function with that name.
 */
%start goal
%type <Syntax.compilation_unit> goal

%%

goal:
	CompilationUnit EOF  { $1 }
;

/* 3.8 */

Identifier:
	IDENTIFIER  { $1 }
;

/* 3.10 */

Literal:
	LITERAL  { $1 }
;

/* 4.1 */

Type:
	PrimitiveType  { $1 }
|	ReferenceType  { $1 }
;

/* 4.2 */

PrimitiveType:
	PRIMITIVE_TYPE  { TypeName [$1] }
;

/* 4.3 */

ReferenceType:
	ClassOrInterfaceType  { TypeName $1 }
|	ArrayType  { $1 }
;

ClassOrInterfaceType:
	Name  { $1 }
;

ClassType:
	Name  { $1 }
;

InterfaceType:
	Name  { $1 }
;

ArrayType:
	PrimitiveType LB RB  { ArrayType $1 }
|	Name LB RB  { ArrayType (TypeName $1) }
|	ArrayType LB RB  { ArrayType $1 }
;

/* 6.5 */

Name:
	Identifier  { [$1] }
|	Name DOT Identifier  { $1 @ [$3] }
;

/* 7.3 */

CompilationUnit:
	PackageDeclarationOpt ImportDeclarationsOpt TypeDeclarationsOpt
	{ compilation_unit ($1, $2, $3) }
;

ImportDeclarations:
	ImportDeclaration  { [$1] }
|	ImportDeclarations ImportDeclaration  { $1 @ [$2] }
;

ImportDeclarationsOpt:
	/* empty */  { [] }
|	ImportDeclarations  { $1 }
;

TypeDeclarations:
	TypeDeclaration  { $1 }
|	TypeDeclarations TypeDeclaration  { $1 @ $2 }
;

TypeDeclarationsOpt:
	/* empty */  { [] }
|	TypeDeclarations  { $1 }
;

/* 7.4.1 */

PackageDeclaration:
	PACKAGE Name SM  { $2 }
;

PackageDeclarationOpt:
	/* empty */  { None }
|	PackageDeclaration  { Some $1 }
;

/* 7.5 */

ImportDeclaration:
	SingleTypeImportDeclaration  { $1 }
|	TypeImportOnDemandDeclaration  { $1 }
;

/* 7.5.1 */

SingleTypeImportDeclaration:
	IMPORT Name SM  { $2 }
;

/* 7.5.2 */

TypeImportOnDemandDeclaration:
	IMPORT Name DOT TIMES SM  { $2 @ ["*"] }
;

/* 7.6 */

TypeDeclaration:
	ClassDeclaration  { [Class $1] }
|	InterfaceDeclaration  { [Interface $1] }
|	SM  { [] }
;

/* 8.1 */

ClassDeclaration:
	ModifiersOpt CLASS Identifier SuperOpt InterfacesOpt ClassBody
		{ class_decl ($1, $3, $4, $5, $6) }
;

/* 8.1.1 */
/* 8.3.1 */
/* 8.4.3 */
/* 8.8.3 */
/* 9.1.1 */
/* 9.3 */
/* 9.4 */

/*
 * To avoid shift/reduce conflicts, we accept all modifiers
 * in front of all declarations.  The ones not applicable to
 * a particular kind of declaration must be detected in semantic actions.
 */

Modifiers:
	Modifier  { [$1] }
|	Modifiers Modifier  { $1 @ [$2] }
;

ModifiersOpt:
	/* empty */  { [] }
|	Modifiers  { $1 }
;

Modifier:
	PUBLIC  { Public }
|	PROTECTED  { Protected }
|	PRIVATE  { Private }
|	ABSTRACT  { Abstract }
|	STATIC  { Static }
|	FINAL  { Final }
|	STRICTFP  { StrictFP }
|	TRANSIENT  { Transient }
|	VOLATILE  { Volatile }
|	SYNCHRONIZED  { Synchronized }
|	NATIVE  { Native }
;

/* 8.1.3 */

Super:
	EXTENDS ClassType  { $2 }
;

SuperOpt:
	/* empty */  { None }
|	Super  { Some $1 }
;

/* 8.1.4 */

Interfaces:
	IMPLEMENTS InterfaceTypeList  { $2 }
;

InterfacesOpt:
	/* empty */  { [] }
|	Interfaces  { $1 }
;

InterfaceTypeList:
	InterfaceType  { [$1] }
|	InterfaceTypeList CM InterfaceType  { $1 @ [$3] }
;

/* 8.1.5 */

ClassBody:
	LC ClassBodyDeclarationsOpt RC  { $2 }
;

ClassBodyDeclarations:
	ClassBodyDeclaration  { $1 }
|	ClassBodyDeclarations ClassBodyDeclaration  { $1 @ $2 }
;

ClassBodyDeclarationsOpt:
	/* empty */  { [] }
|	ClassBodyDeclarations  { $1 }
;

ClassBodyDeclaration:
	ClassMemberDeclaration  { $1 }
|	InstanceInitializer  { [$1] }
|	StaticInitializer  { [$1] }
|	ConstructorDeclaration  { [$1] }
;

ClassMemberDeclaration:
	FieldDeclaration  { $1 }
|	MethodDeclaration  { [Method $1] }
|	ClassDeclaration  { [Class $1] }
|	InterfaceDeclaration  { [Interface $1] }
|	SM  { [] }
;

/* 8.3 */

FieldDeclaration:
	ModifiersOpt Type VariableDeclarators SM  { field_decls ($1, $2, $3) }
;

VariableDeclarators:
	VariableDeclarator  { [$1] }
|	VariableDeclarators CM VariableDeclarator  { $1 @ [$3] }
;

VariableDeclarator:
	VariableDeclaratorId  { $1, None }
|	VariableDeclaratorId EQ VariableInitializer  { $1, Some $3 }
;

VariableDeclaratorId:
	Identifier  { IdentDecl $1 }
|	VariableDeclaratorId LB RB  { ArrayDecl $1 }
;

VariableInitializer:
	Expression  { ExprInit $1 }
|	ArrayInitializer  { $1 }
;

/* 8.4 */

MethodDeclaration:
	MethodHeader MethodBody  { method_decl ($1, $2) }
;

MethodHeader:
	ModifiersOpt Type MethodDeclarator ThrowsOpt
		{ method_header ($1, $2, $3, $4) }
|	ModifiersOpt VOID MethodDeclarator ThrowsOpt
		{ method_header ($1, TypeName ["void"], $3, $4) }
;

MethodDeclarator:
	Identifier LP FormalParameterListOpt RP  { IdentDecl $1, $3 }
|	MethodDeclarator LB RB  { ArrayDecl (fst $1), snd $1 }
;

/* 8.4.1 */

FormalParameterList:
	FormalParameter  { [$1] }
|	FormalParameterList CM FormalParameter  { $1 @ [$3] }
;

FormalParameterListOpt:
	/* empty */  { [] }
|	FormalParameterList  { $1 }
;

FormalParameter:
	FinalOpt Type VariableDeclaratorId  { formal_decl ($1, $2, $3) }
;

FinalOpt:
	/* empty */  { [] }
|	FINAL  { [Final] }
;

/* 8.4.4 */

Throws:
	THROWS ClassTypeList  { $2 }
;

ThrowsOpt:
	/* empty */  { [] }
|	Throws  { $1 }
;

ClassTypeList:
	ClassType  { [$1] }
|	ClassTypeList CM ClassType  { $1 @ [$3] }
;

/* 8.4.5 */

MethodBody:
	Block  { $1 }
|	SM  { Empty }
;

/* 8.6 */

InstanceInitializer:
	Block  { InstanceInit $1 }
;

/* 8.7 */

StaticInitializer:
	STATIC Block  { StaticInit $2 }
;

/* 8.8 */

ConstructorDeclaration:
	ModifiersOpt ConstructorDeclarator ThrowsOpt ConstructorBody
		{ constructor ($1, $2, $3, $4) }
;

ConstructorDeclarator:
	Identifier LP FormalParameterListOpt RP  { $1, $3 }
;

/* 8.8.5 */

ConstructorBody:
	LC BlockStatementsOpt RC  { Block $2 }
|	LC ExplicitConstructorInvocation BlockStatementsOpt RC
		{ Block ($2 :: $3) }
;

/* 8.8.5.1 */

ExplicitConstructorInvocation:
	THIS LP ArgumentListOpt RP SM
		{ Expr (Call (Name ["this"], $3)) }
|	SUPER LP ArgumentListOpt RP SM
		{ Expr (Call (Name ["super"], $3)) }
|	Primary DOT SUPER LP ArgumentListOpt RP SM
		{ Expr (Call (Dot ($1, "super"), $5)) }
;

/* 9.1 */

InterfaceDeclaration:
	ModifiersOpt INTERFACE Identifier
		ExtendsInterfacesOpt InterfaceBody
			{ interface_decl ($1, $3, $4, $5) }
;

/* 9.1.2 */

ExtendsInterfaces:
	EXTENDS InterfaceType  { [$2] }
|	ExtendsInterfaces CM InterfaceType  { $1 @ [$3] }
;

ExtendsInterfacesOpt:
	/* empty */  { [] }
|	ExtendsInterfaces  { $1 }
;

/* 9.1.3 */

InterfaceBody:
	LC InterfaceMemberDeclarationsOpt RC  { $2 }
;

InterfaceMemberDeclarations:
	InterfaceMemberDeclaration  { $1 }
|	InterfaceMemberDeclarations InterfaceMemberDeclaration  { $1 @ $2 }
;

InterfaceMemberDeclarationsOpt:
	/* empty */  { [] }
|	InterfaceMemberDeclarations  { $1 }
;

InterfaceMemberDeclaration:
	ConstantDeclaration  { $1 }
|	AbstractMethodDeclaration  { [Method $1] }
|	ClassDeclaration  { [Class $1] }
|	InterfaceDeclaration  { [Interface $1] }
|	SM  { [] }
;

/* 9.3 */

/*
 * Note: final semicolon was missing from draft 2nd edition spec.
 */

ConstantDeclaration:
	ModifiersOpt Type VariableDeclarators SM
		{ field_decls ($1, $2, $3) }
;

/* 9.4 */

AbstractMethodDeclaration:
	ModifiersOpt Type MethodDeclarator ThrowsOpt SM
		{ method_header ($1, $2, $3, $4) }
|	ModifiersOpt VOID MethodDeclarator ThrowsOpt SM
		{ method_header ($1, TypeName ["void"], $3, $4) }
;

/* 10.6 */

ArrayInitializer:
	LC CommaOpt RC  { ArrayInit [] }
|	LC VariableInitializers CommaOpt RC  { ArrayInit $2 }
;

VariableInitializers:
	VariableInitializer  { [$1] }
|	VariableInitializers CM VariableInitializer  { $1 @ [$3] }
;

CommaOpt:
	/* empty */  { () }
|	CM  { () }
;

/* 14.2 */

Block:
	LC BlockStatementsOpt RC  { Block $2 }
;

BlockStatements:
	BlockStatement  { $1 }
|	BlockStatements BlockStatement  { $1 @ $2 }
;

BlockStatementsOpt:
	/* empty */  { [] }
|	BlockStatements  { $1 }
;

BlockStatement:
	LocalVariableDeclarationStatement  { $1 }
|	ClassDeclaration  { [LocalClass $1] }
|	Statement  { [$1] }
;

/* 14.4 */

LocalVariableDeclarationStatement:
	LocalVariableDeclaration SM  { $1 }
;

LocalVariableDeclaration:
	Type VariableDeclarators  { var_decls ([], $1, $2) }
|	FINAL Type VariableDeclarators  { var_decls ([Final], $2, $3) }
;

/* 14.5 */

Statement:
	StatementWithoutTrailingSubstatement  { $1 }
|	LabeledStatement  { $1 }
|	IfThenStatement  { $1 }
|	IfThenElseStatement  { $1 }
|	WhileStatement  { $1 }
|	ForStatement  { $1 }
;

StatementNoShortIf:
	StatementWithoutTrailingSubstatement  { $1 }
|	LabeledStatementNoShortIf  { $1 }
|	IfThenElseStatementNoShortIf  { $1 }
|	WhileStatementNoShortIf  { $1 }
|	ForStatementNoShortIf  { $1 }
;

StatementWithoutTrailingSubstatement:
	Block  { $1 }
|	EmptyStatement  { $1 }
|	ExpressionStatement  { $1 }
|	SwitchStatement  { $1 }
|	DoStatement  { $1 }
|	BreakStatement  { $1 }
|	ContinueStatement  { $1 }
|	ReturnStatement  { $1 }
|	SynchronizedStatement  { $1 }
|	ThrowStatement  { $1 }
|	TryStatement  { $1 }
;

/* 14.6 */

EmptyStatement:
	SM  { Empty }
;

/* 14.7 */

LabeledStatement:
	Identifier COLON Statement  { Label ($1, $3) }
;

LabeledStatementNoShortIf:
	Identifier COLON StatementNoShortIf  { Label ($1, $3) }
;

/* 14.8 */

ExpressionStatement:
	StatementExpression SM  { Expr $1 }
;

StatementExpression:
	Assignment  { $1 }
|	PreIncrementExpression  { $1 }
|	PreDecrementExpression  { $1 }
|	PostIncrementExpression  { $1 }
|	PostDecrementExpression  { $1 }
|	MethodInvocation  { $1 }
|	ClassInstanceCreationExpression  { $1 }
;

/* 14.9 */

IfThenStatement:
	IF LP Expression RP Statement
		{ If ($3, $5, Empty) }
;

IfThenElseStatement:
	IF LP Expression RP StatementNoShortIf ELSE Statement
		{ If ($3, $5, $7) }
;

IfThenElseStatementNoShortIf:
	IF LP Expression RP StatementNoShortIf ELSE StatementNoShortIf
		{ If ($3, $5, $7) }
;

/* 14.10 */

SwitchStatement:
	SWITCH LP Expression RP SwitchBlock
		{ Switch ($3, $5) }
;

SwitchBlock:
	LC RC  { [] }
|	LC SwitchLabels RC  { [$2, []] }
|	LC SwitchBlockStatementGroups RC  { $2 }
|	LC SwitchBlockStatementGroups SwitchLabels RC  { $2 @ [$3, []] }
;

SwitchBlockStatementGroups:
	SwitchBlockStatementGroup  { [$1] }
|	SwitchBlockStatementGroups SwitchBlockStatementGroup  { $1 @ [$2] }
;

SwitchBlockStatementGroup:
	SwitchLabels BlockStatements  { $1, $2 }
;

SwitchLabels:
	SwitchLabel  { [$1] }
|	SwitchLabels SwitchLabel  { $1 @ [$2] }
;

SwitchLabel:
	CASE ConstantExpression COLON  { Case $2 }
|	DEFAULT COLON  { Default }
;

/* 14.11 */

WhileStatement:
	WHILE LP Expression RP Statement
		{ While ($3, $5) }
;

WhileStatementNoShortIf:
	WHILE LP Expression RP StatementNoShortIf
		{ While ($3, $5) }
;

/* 14.12 */

DoStatement:
	DO Statement WHILE LP Expression RP SM
		{ Do ($2, $5) }
;

/* 14.13 */

ForStatement:
	FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
		Statement
			{ For ($3, $5, $7, $9) }
;

ForStatementNoShortIf:
	FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
		StatementNoShortIf
			{ For ($3, $5, $7, $9) }
;

ForInit:
	StatementExpressionList  { $1 }
|	LocalVariableDeclaration  { $1 }
;

ForInitOpt:
	/* empty */  { [] }
|	ForInit  { $1 }
;

ExpressionOpt:
	/* empty */  { None }
|	Expression  { Some $1 }
;

ForUpdate:
	StatementExpressionList  { $1 }
;

ForUpdateOpt:
	/* empty */  { [] }
|	ForUpdate  { $1 }
;

StatementExpressionList:
	StatementExpression  { [Expr $1] }
|	StatementExpressionList CM StatementExpression  { $1 @ [Expr $3] }
;

/* 14.14 */

BreakStatement:
	BREAK IdentifierOpt SM  { Break $2 }
;

IdentifierOpt:
	/* empty */  { None }
|	Identifier  { Some $1 }
;

/* 14.15 */

ContinueStatement:
	CONTINUE IdentifierOpt SM  { Continue $2 }
;

/* 14.16 */

ReturnStatement:
	RETURN ExpressionOpt SM  { Return $2 }
;

/* 14.17 */

ThrowStatement:
	THROW Expression SM  { Throw $2 }
;

/* 14.18 */

SynchronizedStatement:
	SYNCHRONIZED LP Expression RP Block  { Sync ($3, $5) }
;

/* 14.19 */

TryStatement:
	TRY Block Catches  { Try ($2, $3, None) }
|	TRY Block CatchesOpt Finally  { Try ($2, $3, Some $4) }
;

Catches:
	CatchClause  { [$1] }
|	Catches CatchClause  { $1 @ [$2] }
;

CatchesOpt:
	/* empty */  { [] }
|	Catches  { $1 }
;

CatchClause:
	CATCH LP FormalParameter RP Block  { $3, $5 }
;

Finally:
	FINALLY Block  { $2 }
;

/* 15.8 */

Primary:
	PrimaryNoNewArray  { $1 }
|	ArrayCreationExpression  { $1 }
;

PrimaryNoNewArray:
	Literal  { Literal $1 }
|	ClassLiteral  { $1 }
|	THIS  { Name ["this"] }
|	Name DOT THIS  { Dot (Name $1, "this") }
|	LP Expression RP  { $2 }
|	ClassInstanceCreationExpression  { $1 }
|	FieldAccess  { $1 }
|	MethodInvocation  { $1 }
|	ArrayAccess  { $1 }
;

/* 15.8.2 */

ClassLiteral:
	PrimitiveType DOT CLASS  { ClassLiteral $1 }
|	Name DOT CLASS  { ClassLiteral (TypeName $1) }
|	ArrayType DOT CLASS  { ClassLiteral $1 }
|	VOID DOT CLASS  { ClassLiteral (TypeName ["void"]) }
;

/* 15.9 */

ClassInstanceCreationExpression:
	NEW ClassOrInterfaceType LP ArgumentListOpt RP ClassBodyOpt
		{ NewClass (TypeName $2, $4, $6) }
|	Primary DOT NEW Identifier LP ArgumentListOpt RP ClassBodyOpt
		{ NewQualifiedClass ($1, $4, $6, $8) }
;

ArgumentList:
	Expression  { [$1] }
|	ArgumentList CM Expression  { $1 @ [$3] }
;

ArgumentListOpt:
	/* empty */  { [] }
|	ArgumentList  { $1 }
;

ClassBodyOpt:
	/* empty */  { None }
|	ClassBody  { Some $1 }
;

/* 15.10 */

ArrayCreationExpression:
	NEW PrimitiveType DimExprs DimsOpt
		{ NewArray ($2, $3, $4, None) }
|	NEW Name DimExprs DimsOpt
		{ NewArray (TypeName $2, $3, $4, None) }
|	NEW PrimitiveType Dims ArrayInitializer
		{ NewArray ($2, [], $3, Some $4) }
|	NEW Name Dims ArrayInitializer
		{ NewArray (TypeName $2, [], $3, Some $4) }
;

DimExprs:
	DimExpr  { [$1] }
|	DimExprs DimExpr  { $1 @ [$2] }
;

DimExpr:
	LB Expression RB  { $2 }
;

Dims:
	LB RB  { 1 }
|	Dims LB RB  { $1 + 1 }
;

DimsOpt:
	/* empty */  { 0 }
|	Dims  { $1 }
;

/* 15.11 */

FieldAccess:
	Primary DOT Identifier
		{ Dot ($1, $3) }
|	SUPER DOT Identifier
		{ Dot (Name ["super"], $3) }
|	Name DOT SUPER DOT Identifier
		{ Dot (Dot (Name $1, "super"), $5) }
;

/* 15.12 */

MethodInvocation:
	Name LP ArgumentListOpt RP  { Call (Name $1, $3) }
|	Primary DOT Identifier LP ArgumentListOpt RP
		{ Call (Dot ($1, $3), $5) }
|	SUPER DOT Identifier LP ArgumentListOpt RP
		{ Call (Dot (Name ["super"], $3), $5) }
|	Name DOT SUPER DOT Identifier LP ArgumentListOpt RP
		{ Call (Dot (Dot (Name $1, "super"), $5), $7) }
;

/* 15.13 */

ArrayAccess:
	Name LB Expression RB  { Infix (Name $1, "[]", $3) }
|	PrimaryNoNewArray LB Expression RB  { Infix ($1, "[]", $3) }
;

/* 15.14 */

PostfixExpression:
	Primary  { $1 }
|	Name  { Name $1 }
|	PostIncrementExpression  { $1 }
|	PostDecrementExpression  { $1 }
;

/* 15.14.1 */

PostIncrementExpression:
	PostfixExpression INCR  { Postfix ($1, "++") }
;

/* 15.14.2 */

PostDecrementExpression:
	PostfixExpression DECR  { Postfix ($1, "--") }
;

/* 15.15 */

UnaryExpression:
	PreIncrementExpression  { $1 }
|	PreDecrementExpression  { $1 }
|	PLUS UnaryExpression  { Prefix ("+", $2) }
|	MINUS UnaryExpression  { Prefix ("-", $2) }
|	UnaryExpressionNotPlusMinus  { $1 }
;

PreIncrementExpression:
	INCR UnaryExpression  { Prefix ("++", $2) }
;

PreDecrementExpression:
	DECR UnaryExpression  { Prefix ("--", $2) }
;

UnaryExpressionNotPlusMinus:
	PostfixExpression  { $1 }
|	COMPL UnaryExpression  { Prefix ("~", $2) }
|	NOT UnaryExpression  { Prefix ("!", $2) }
|	CastExpression  { $1 }
;

/* 15.16 */

/* Original rule:

CastExpression:
	LP PrimitiveType DimsOpt RP UnaryExpression
|	LP ReferenceType RP UnaryExpressionNotPlusMinus
;

*/

/*
 * Modified (overly liberal) rule for LALR(1) grammar.
 * Semantic action must ensure that '( Expression )' is really '( Name )'
 */

CastExpression:
	LP PrimitiveType RP UnaryExpression  { Cast ($2, $4) }
|	LP Expression RP UnaryExpressionNotPlusMinus
		{ Cast (type_name $2, $4) }
|	LP ArrayType RP UnaryExpressionNotPlusMinus  { Cast ($2, $4) }
;

/* 15.17 */

MultiplicativeExpression:
	UnaryExpression  { $1 }
|	MultiplicativeExpression TIMES UnaryExpression
		{ Infix ($1, "*", $3) }
|	MultiplicativeExpression DIV UnaryExpression
		{ Infix ($1, "/", $3) }
|	MultiplicativeExpression MOD UnaryExpression
		{ Infix ($1, "%", $3) }
;

/* 15.18 */

AdditiveExpression:
	MultiplicativeExpression  { $1 }
|	AdditiveExpression PLUS MultiplicativeExpression
		{ Infix ($1, "+", $3) }
|	AdditiveExpression MINUS MultiplicativeExpression
		{ Infix ($1, "-", $3) }
;

/* 15.19 */

ShiftExpression:
	AdditiveExpression  { $1 }
|	ShiftExpression LS AdditiveExpression  { Infix ($1, "<<", $3) }
|	ShiftExpression SRS AdditiveExpression  { Infix ($1, ">>", $3) }
|	ShiftExpression URS AdditiveExpression  { Infix ($1, ">>>", $3) }
;

/* 15.20 */

RelationalExpression:
	ShiftExpression  { $1 }
|	RelationalExpression LT ShiftExpression  { Infix ($1, "<", $3) }
|	RelationalExpression GT ShiftExpression  { Infix ($1, ">", $3) }
|	RelationalExpression LE ShiftExpression  { Infix ($1, "<=", $3) }
|	RelationalExpression GE ShiftExpression  { Infix ($1, ">=", $3) }
|	RelationalExpression INSTANCEOF ReferenceType  { InstanceOf ($1, $3) }
;

/* 15.21 */

EqualityExpression:
	RelationalExpression  { $1 }
|	EqualityExpression EQ_EQ RelationalExpression
		{ Infix ($1, "==", $3) }
|	EqualityExpression NOT_EQ RelationalExpression
		{ Infix ($1, "!=", $3) }
;

/* 15.22 */

AndExpression:
	EqualityExpression  { $1 }
|	AndExpression AND EqualityExpression  { Infix ($1, "&", $3) }
;

ExclusiveOrExpression:
	AndExpression  { $1 }
|	ExclusiveOrExpression XOR AndExpression  { Infix ($1, "^", $3) }
;

InclusiveOrExpression:
	ExclusiveOrExpression  { $1 }
|	InclusiveOrExpression OR ExclusiveOrExpression  { Infix ($1, "|", $3) }
;

/* 15.23 */

ConditionalAndExpression:
	InclusiveOrExpression  { $1 }
|	ConditionalAndExpression AND_AND InclusiveOrExpression
		{ Infix ($1, "&&", $3) }
;

/* 15.24 */

ConditionalOrExpression:
	ConditionalAndExpression  { $1 }
|	ConditionalOrExpression OR_OR ConditionalAndExpression
		{ Infix ($1, "||", $3) }
;

/* 15.25 */

ConditionalExpression:
	ConditionalOrExpression  { $1 }
|	ConditionalOrExpression COND Expression COLON ConditionalExpression
		{ Conditional ($1, $3, $5) }
;

/* 15.26 */

AssignmentExpression:
	ConditionalExpression  { $1 }
|	Assignment  { $1 }
;

Assignment:
	LeftHandSide AssignmentOperator AssignmentExpression
		{ Infix ($1, $2, $3) }
;

LeftHandSide:
	Name  { Name $1 }
|	FieldAccess  { $1 }
|	ArrayAccess  { $1 }
;

AssignmentOperator:
	EQ  { "=" }
|	OPERATOR_EQ  { $1 }
;

/* 15.27 */

Expression:
	AssignmentExpression  { $1 }
;

/* 15.28 */

ConstantExpression:
	Expression  { $1 }
;
