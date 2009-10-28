/* Joust: a Java lexer, parser, and pretty-printer written in OCaml
   Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License */

/* LALR(1) (yacc/bison) grammar for Java

   Attempts to conform to:

   The Java Language Specification
   Second Edition

   James Gosling, Bill Joy, Guy Steele, Gilad Bracha */

%token IDENTIFIER
%token LITERAL
%token PRIMITIVE_TYPE

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
%token OPERATOR_EQ	/* += -= *= /= &= |= ^= %= <<= >>= >>>= */

/*
 * 3.9 Keywords
 */
%token ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE
%token DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO
%token IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG
%token NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN
%token SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE

%start Goal

%%

Goal:
	CompilationUnit
;

/* 3.8 */

Identifier:
	IDENTIFIER
;

/* 3.10 */

Literal:
	LITERAL
;

/* 4.1 */

Type:
	PrimitiveType
|	ReferenceType
;

/* 4.2 */

PrimitiveType:
	PRIMITIVE_TYPE
;

/* 4.3 */

ReferenceType:
	ClassOrInterfaceType
|	ArrayType
;

ClassOrInterfaceType:
	Name
;

ClassType:
	Name
;

InterfaceType:
	Name
;

ArrayType:
	PrimitiveType LB RB
|	Name LB RB
|	ArrayType LB RB
;

/* 6.5 */

Name:
	Identifier
|	Name DOT Identifier
;

/* 7.3 */

CompilationUnit:
	PackageDeclarationOpt ImportDeclarationsOpt TypeDeclarationsOpt

;

ImportDeclarations:
	ImportDeclaration
|	ImportDeclarations ImportDeclaration
;

ImportDeclarationsOpt:
	/* empty */
|	ImportDeclarations
;

TypeDeclarations:
	TypeDeclaration
|	TypeDeclarations TypeDeclaration
;

TypeDeclarationsOpt:
	/* empty */
|	TypeDeclarations
;

/* 7.4.1 */

PackageDeclaration:
	PACKAGE Name SM
;

PackageDeclarationOpt:
	/* empty */
|	PackageDeclaration
;

/* 7.5 */

ImportDeclaration:
	SingleTypeImportDeclaration
|	TypeImportOnDemandDeclaration
;

/* 7.5.1 */

SingleTypeImportDeclaration:
	IMPORT Name SM
;

/* 7.5.2 */

TypeImportOnDemandDeclaration:
	IMPORT Name DOT TIMES SM
;

/* 7.6 */

TypeDeclaration:
	ClassDeclaration
|	InterfaceDeclaration
|	SM
;

/* 8.1 */

ClassDeclaration:
	ModifiersOpt CLASS Identifier SuperOpt InterfacesOpt ClassBody

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
	Modifier
|	Modifiers Modifier
;

ModifiersOpt:
	/* empty */
|	Modifiers
;

Modifier:
	PUBLIC
|	PROTECTED
|	PRIVATE
|	ABSTRACT
|	STATIC
|	FINAL
|	STRICTFP
|	TRANSIENT
|	VOLATILE
|	SYNCHRONIZED
|	NATIVE
;

/* 8.1.3 */

Super:
	EXTENDS ClassType
;

SuperOpt:
	/* empty */
|	Super
;

/* 8.1.4 */

Interfaces:
	IMPLEMENTS InterfaceTypeList
;

InterfacesOpt:
	/* empty */
|	Interfaces
;

InterfaceTypeList:
	InterfaceType
|	InterfaceTypeList CM InterfaceType
;

/* 8.1.5 */

ClassBody:
	LC ClassBodyDeclarationsOpt RC
;

ClassBodyDeclarations:
	ClassBodyDeclaration
|	ClassBodyDeclarations ClassBodyDeclaration
;

ClassBodyDeclarationsOpt:
	/* empty */
|	ClassBodyDeclarations
;

ClassBodyDeclaration:
	ClassMemberDeclaration
|	InstanceInitializer
|	StaticInitializer
|	ConstructorDeclaration
;

ClassMemberDeclaration:
	FieldDeclaration
|	MethodDeclaration
|	ClassDeclaration
|	InterfaceDeclaration
|	SM
;

/* 8.3 */

FieldDeclaration:
	ModifiersOpt Type VariableDeclarators SM
;

VariableDeclarators:
	VariableDeclarator
|	VariableDeclarators CM VariableDeclarator
;

VariableDeclarator:
	VariableDeclaratorId
|	VariableDeclaratorId EQ VariableInitializer
;

VariableDeclaratorId:
	Identifier
|	VariableDeclaratorId LB RB
;

VariableInitializer:
	Expression
|	ArrayInitializer
;

/* 8.4 */

MethodDeclaration:
	MethodHeader MethodBody
;

MethodHeader:
	ModifiersOpt Type MethodDeclarator ThrowsOpt

|	ModifiersOpt VOID MethodDeclarator ThrowsOpt

;

MethodDeclarator:
	Identifier LP FormalParameterListOpt RP
|	MethodDeclarator LB RB
;

/* 8.4.1 */

FormalParameterList:
	FormalParameter
|	FormalParameterList CM FormalParameter
;

FormalParameterListOpt:
	/* empty */
|	FormalParameterList
;

FormalParameter:
	FinalOpt Type VariableDeclaratorId
;

FinalOpt:
	/* empty */
|	FINAL
;

/* 8.4.4 */

Throws:
	THROWS ClassTypeList
;

ThrowsOpt:
	/* empty */
|	Throws
;

ClassTypeList:
	ClassType
|	ClassTypeList CM ClassType
;

/* 8.4.5 */

MethodBody:
	Block
|	SM
;

/* 8.6 */

InstanceInitializer:
	Block
;

/* 8.7 */

StaticInitializer:
	STATIC Block
;

/* 8.8 */

ConstructorDeclaration:
	ModifiersOpt ConstructorDeclarator ThrowsOpt ConstructorBody

;

ConstructorDeclarator:
	Identifier LP FormalParameterListOpt RP
;

/* 8.8.5 */

ConstructorBody:
	LC BlockStatementsOpt RC
|	LC ExplicitConstructorInvocation BlockStatementsOpt RC

;

/* 8.8.5.1 */

ExplicitConstructorInvocation:
	THIS LP ArgumentListOpt RP SM

|	SUPER LP ArgumentListOpt RP SM

|	Primary DOT SUPER LP ArgumentListOpt RP SM

	/*
	 * Not in 2nd edition Java Language Specification.
	 */
|	Name DOT SUPER LP ArgumentListOpt RP SM

;

/* 9.1 */

InterfaceDeclaration:
	ModifiersOpt INTERFACE Identifier
		ExtendsInterfacesOpt InterfaceBody

;

/* 9.1.2 */

ExtendsInterfaces:
	EXTENDS InterfaceType
|	ExtendsInterfaces CM InterfaceType
;

ExtendsInterfacesOpt:
	/* empty */
|	ExtendsInterfaces
;

/* 9.1.3 */

InterfaceBody:
	LC InterfaceMemberDeclarationsOpt RC
;

InterfaceMemberDeclarations:
	InterfaceMemberDeclaration
|	InterfaceMemberDeclarations InterfaceMemberDeclaration
;

InterfaceMemberDeclarationsOpt:
	/* empty */
|	InterfaceMemberDeclarations
;

InterfaceMemberDeclaration:
	ConstantDeclaration
|	AbstractMethodDeclaration
|	ClassDeclaration
|	InterfaceDeclaration
|	SM
;

/* 9.3 */

/*
 * Note: semicolon is missing in 2nd edition Java Language Specification.
 */

ConstantDeclaration:
	ModifiersOpt Type VariableDeclarators SM
;

/* 9.4 */

AbstractMethodDeclaration:
	ModifiersOpt Type MethodDeclarator ThrowsOpt SM
|	ModifiersOpt VOID MethodDeclarator ThrowsOpt SM

;

/* 10.6 */

ArrayInitializer:
	LC CommaOpt RC
|	LC VariableInitializers CommaOpt RC
;

VariableInitializers:
	VariableInitializer
|	VariableInitializers CM VariableInitializer
;

CommaOpt:
	/* empty */
|	CM
;

/* 14.2 */

Block:
	LC BlockStatementsOpt RC
;

BlockStatements:
	BlockStatement
|	BlockStatements BlockStatement
;

BlockStatementsOpt:
	/* empty */
|	BlockStatements
;

BlockStatement:
	LocalVariableDeclarationStatement
|	ClassDeclaration
|	Statement
;

/* 14.4 */

LocalVariableDeclarationStatement:
	LocalVariableDeclaration SM
;

LocalVariableDeclaration:
	Type VariableDeclarators
|	FINAL Type VariableDeclarators
;

/* 14.5 */

Statement:
	StatementWithoutTrailingSubstatement
|	LabeledStatement
|	IfThenStatement
|	IfThenElseStatement
|	WhileStatement
|	ForStatement
;

StatementNoShortIf:
	StatementWithoutTrailingSubstatement
|	LabeledStatementNoShortIf
|	IfThenElseStatementNoShortIf
|	WhileStatementNoShortIf
|	ForStatementNoShortIf
;

StatementWithoutTrailingSubstatement:
	Block
|	EmptyStatement
|	ExpressionStatement
|	SwitchStatement
|	DoStatement
|	BreakStatement
|	ContinueStatement
|	ReturnStatement
|	SynchronizedStatement
|	ThrowStatement
|	TryStatement
;

/* 14.6 */

EmptyStatement:
	SM
;

/* 14.7 */

LabeledStatement:
	Identifier COLON Statement
;

LabeledStatementNoShortIf:
	Identifier COLON StatementNoShortIf
;

/* 14.8 */

ExpressionStatement:
	StatementExpression SM
;

StatementExpression:
	Assignment
|	PreIncrementExpression
|	PreDecrementExpression
|	PostIncrementExpression
|	PostDecrementExpression
|	MethodInvocation
|	ClassInstanceCreationExpression
;

/* 14.9 */

IfThenStatement:
	IF LP Expression RP Statement
;

IfThenElseStatement:
	IF LP Expression RP StatementNoShortIf ELSE Statement
;

IfThenElseStatementNoShortIf:
	IF LP Expression RP StatementNoShortIf ELSE StatementNoShortIf
;

/* 14.10 */

SwitchStatement:
	SWITCH LP Expression RP SwitchBlock
;

SwitchBlock:
	LC RC
|	LC SwitchLabels RC
|	LC SwitchBlockStatementGroups RC
|	LC SwitchBlockStatementGroups SwitchLabels RC
;

SwitchBlockStatementGroups:
	SwitchBlockStatementGroup
|	SwitchBlockStatementGroups SwitchBlockStatementGroup
;

SwitchBlockStatementGroup:
	SwitchLabels BlockStatements
;

SwitchLabels:
	SwitchLabel
|	SwitchLabels SwitchLabel
;

SwitchLabel:
	CASE ConstantExpression COLON
|	DEFAULT COLON
;

/* 14.11 */

WhileStatement:
	WHILE LP Expression RP Statement
;

WhileStatementNoShortIf:
	WHILE LP Expression RP StatementNoShortIf
;

/* 14.12 */

DoStatement:
	DO Statement WHILE LP Expression RP SM
;

/* 14.13 */

ForStatement:
	FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
		Statement
;

ForStatementNoShortIf:
	FOR LP ForInitOpt SM ExpressionOpt SM ForUpdateOpt RP
		StatementNoShortIf
;

ForInit:
	StatementExpressionList
|	LocalVariableDeclaration
;

ForInitOpt:
	/* empty */
|	ForInit
;

ExpressionOpt:
	/* empty */
|	Expression
;

ForUpdate:
	StatementExpressionList
;

ForUpdateOpt:
	/* empty */
|	ForUpdate
;

StatementExpressionList:
	StatementExpression
|	StatementExpressionList CM StatementExpression
;

/* 14.14 */

BreakStatement:
	BREAK IdentifierOpt SM
;

IdentifierOpt:
	/* empty */
|	Identifier
;

/* 14.15 */

ContinueStatement:
	CONTINUE IdentifierOpt SM
;

/* 14.16 */

ReturnStatement:
	RETURN ExpressionOpt SM
;

/* 14.17 */

ThrowStatement:
	THROW Expression SM
;

/* 14.18 */

SynchronizedStatement:
	SYNCHRONIZED LP Expression RP Block
;

/* 14.19 */

TryStatement:
	TRY Block Catches
|	TRY Block CatchesOpt Finally
;

Catches:
	CatchClause
|	Catches CatchClause
;

CatchesOpt:
	/* empty */
|	Catches
;

CatchClause:
	CATCH LP FormalParameter RP Block
	/*
	 * Not in 2nd edition Java Language Specification.
	 */
|	CATCH LP FormalParameter RP EmptyStatement
;

Finally:
	FINALLY Block
;

/* 15.8 */

Primary:
	PrimaryNoNewArray
|	ArrayCreationExpression
;

PrimaryNoNewArray:
	Literal
|	ClassLiteral
|	THIS
|	Name DOT THIS
|	LP Expression RP
|	ClassInstanceCreationExpression
|	FieldAccess
|	MethodInvocation
|	ArrayAccess
;

/* 15.8.2 */

ClassLiteral:
	PrimitiveType DOT CLASS
|	Name DOT CLASS
|	ArrayType DOT CLASS
|	VOID DOT CLASS
;

/* 15.9 */

ClassInstanceCreationExpression:
	NEW ClassOrInterfaceType LP ArgumentListOpt RP ClassBodyOpt
|	Primary DOT NEW Identifier LP ArgumentListOpt RP ClassBodyOpt
	/*
	 * Not in 2nd edition Java Language Specification.
	 */
|	Name DOT NEW Identifier LP ArgumentListOpt RP ClassBodyOpt
;

ArgumentList:
	Expression
|	ArgumentList CM Expression
;

ArgumentListOpt:
	/* empty */
|	ArgumentList
;

ClassBodyOpt:
	/* empty */
|	ClassBody
;

/* 15.10 */

ArrayCreationExpression:
	NEW PrimitiveType DimExprs DimsOpt
|	NEW Name DimExprs DimsOpt
|	NEW PrimitiveType Dims ArrayInitializer
|	NEW Name Dims ArrayInitializer
;

DimExprs:
	DimExpr
|	DimExprs DimExpr
;

DimExpr:
	LB Expression RB
;

Dims:
	LB RB
|	Dims LB RB
;

DimsOpt:
	/* empty */
|	Dims
;

/* 15.11 */

FieldAccess:
	Primary DOT Identifier
|	SUPER DOT Identifier
|	Name DOT SUPER DOT Identifier
;

/* 15.12 */

MethodInvocation:
	Name LP ArgumentListOpt RP
|	Primary DOT Identifier LP ArgumentListOpt RP
|	SUPER DOT Identifier LP ArgumentListOpt RP
|	Name DOT SUPER DOT Identifier LP ArgumentListOpt RP
;

/* 15.13 */

ArrayAccess:
	Name LB Expression RB
|	PrimaryNoNewArray LB Expression RB
;

/* 15.14 */

PostfixExpression:
	Primary
|	Name
|	PostIncrementExpression
|	PostDecrementExpression
;

/* 15.14.1 */

PostIncrementExpression:
	PostfixExpression INCR
;

/* 15.14.2 */

PostDecrementExpression:
	PostfixExpression DECR
;

/* 15.15 */

UnaryExpression:
	PreIncrementExpression
|	PreDecrementExpression
|	PLUS UnaryExpression
|	MINUS UnaryExpression
|	UnaryExpressionNotPlusMinus
;

PreIncrementExpression:
	INCR UnaryExpression
;

PreDecrementExpression:
	DECR UnaryExpression
;

UnaryExpressionNotPlusMinus:
	PostfixExpression
|	COMPL UnaryExpression
|	NOT UnaryExpression
|	CastExpression
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
	LP PrimitiveType RP UnaryExpression
|	LP Expression RP UnaryExpressionNotPlusMinus
|	LP ArrayType RP UnaryExpressionNotPlusMinus
;

/* 15.17 */

MultiplicativeExpression:
	UnaryExpression
|	MultiplicativeExpression TIMES UnaryExpression
|	MultiplicativeExpression DIV UnaryExpression
|	MultiplicativeExpression MOD UnaryExpression
;

/* 15.18 */

AdditiveExpression:
	MultiplicativeExpression
|	AdditiveExpression PLUS MultiplicativeExpression
|	AdditiveExpression MINUS MultiplicativeExpression
;

/* 15.19 */

ShiftExpression:
	AdditiveExpression
|	ShiftExpression LS AdditiveExpression
|	ShiftExpression SRS AdditiveExpression
|	ShiftExpression URS AdditiveExpression
;

/* 15.20 */

RelationalExpression:
	ShiftExpression
|	RelationalExpression LT ShiftExpression
|	RelationalExpression GT ShiftExpression
|	RelationalExpression LE ShiftExpression
|	RelationalExpression GE ShiftExpression
|	RelationalExpression INSTANCEOF ReferenceType
;

/* 15.21 */

EqualityExpression:
	RelationalExpression
|	EqualityExpression EQ_EQ RelationalExpression
|	EqualityExpression NOT_EQ RelationalExpression
;

/* 15.22 */

AndExpression:
	EqualityExpression
|	AndExpression AND EqualityExpression
;

ExclusiveOrExpression:
	AndExpression
|	ExclusiveOrExpression XOR AndExpression
;

InclusiveOrExpression:
	ExclusiveOrExpression
|	InclusiveOrExpression OR ExclusiveOrExpression
;

/* 15.23 */

ConditionalAndExpression:
	InclusiveOrExpression
|	ConditionalAndExpression AND_AND InclusiveOrExpression
;

/* 15.24 */

ConditionalOrExpression:
	ConditionalAndExpression
|	ConditionalOrExpression OR_OR ConditionalAndExpression
;

/* 15.25 */

ConditionalExpression:
	ConditionalOrExpression
|	ConditionalOrExpression COND Expression COLON ConditionalExpression
;

/* 15.26 */

AssignmentExpression:
	ConditionalExpression
|	Assignment
;

Assignment:
	LeftHandSide AssignmentOperator AssignmentExpression
;

LeftHandSide:
	Name
|	FieldAccess
|	ArrayAccess
;

AssignmentOperator:
	EQ
|	OPERATOR_EQ
;

/* 15.27 */

Expression:
	AssignmentExpression
;

/* 15.28 */

ConstantExpression:
	Expression
;
