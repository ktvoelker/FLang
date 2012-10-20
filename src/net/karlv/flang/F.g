grammar F;

options {
  language = Java;
  output = AST;
  ASTLabelType = CommonTree;
}

tokens {
  FLAGS;
  DECLS;
  TYPE_PARAMS;
  PARENT;
  MODULE_APP;
  SECTION;
  OP_LEFT;
  OP_RIGHT;
  APP;
  BIND;
  PARAM;
  PARAMS;
  CLAUSE;
  PAT;
  IDENT;
  FNTYPE;
  PRECEDENCE;
  OPEN_QUAL;
  TYPE = 'type';
  VAL = 'val';
  DATA = 'data';
  SIG = 'sig';
  LPAREN = '(';
  RPAREN = ')';
  OPEN = 'open';
  CLOSED = 'closed';
  EXCEPT = 'except';
  ONLY = 'only';
  IS = 'is';
  REC = 'rec';
  LET = 'let';
  FN = 'fn';
  CASE = 'case';
  BEGIN = 'begin';
  DO = 'do';
  TODO = '?';
  WHERE = 'where';
  END = 'end';
  IN = 'in';
  OF = 'of';
  MODULE = 'module';
  WITH = 'with';
  HAS_TYPE = ':';
  SUBTYPE = '<:';
  SUPERTYPE = ':>';
  LARROW = '->';
  RARROW = '<-';
  SEP = ';';
  AUTO = '*';
  DOT = '.';
  FORALL = 'forall';
  INFIX = 'infix';
  LEFT = 'left';
  RIGHT = 'right';
}

@header {
  package net.karlv.flang;
}

@lexer::header {
  package net.karlv.flang;
}

start
  : file^ EOF!
  ;

file
  : module_header^ decls
  | sig_header^ sig_decls
  ;

module_header 
  : MODULE^ ID type_params has_type?
  ;

sig_header 
  : SIG^ ID type_params
  ;

module_app
  : module_prim+ -> ^(MODULE_APP module_prim+)
  ;

module_prim
  : name^
  | LPAREN! module_app^ RPAREN!
  ;
 
decls
  : decl* -> ^(DECLS decl*)
  ;
  
decl 
  : OPEN module_app open_qual? -> ^(OPEN module_app open_qual?)
  | VAL^ bind_name has_type? IS! expr
  | DATA^ data_flags ID type_params parent_type IS! type
  | TYPE^ ID type_params IS! type
  | module_header^ IS! (module_app | decls END!)
  | sig_header^ IS! sig_decls END
  | INFIX^ infix_flags precedence bind_name+
  ;

open_qual
  : mode=(EXCEPT | ONLY) names=(ID | EXPR_OP)+ -> ^(OPEN_QUAL $mode $names)
  ;

infix_flags
  : LEFT -> ^(FLAGS LEFT)
  | RIGHT -> ^(FLAGS RIGHT)
  ;
  
precedence
  : ID -> ^(PRECEDENCE ID)
  | INT -> ^(PRECEDENCE INT)
  ;

data_flags
  : OPEN -> ^(FLAGS OPEN)
  | CLOSED -> ^(FLAGS CLOSED)
  | -> ^(FLAGS)
  ;
  
parent_type
  : SUBTYPE type -> ^(PARENT type)
  | -> ^(PARENT)
  ;

sig_decls
  : sig_decl* -> ^(DECLS sig_decl*)
  ;
  
sig_decl 
  : VAL (ID | EXPR_OP) has_type
  | TYPE ID type_params (type_comp_op type)?
  | MODULE ID type_params has_type
  ;

expr
  : expr_app expr_op* -> ^(OP_LEFT expr_app expr_op*)
  | expr_op+ -> ^(SECTION expr_op+)
  ;

expr_op
  : EXPR_OP expr_app -> ^(OP_RIGHT EXPR_OP expr_app)
  ;

expr_app
  : expr_prim+ -> ^(APP expr_prim+)
  ;

expr_do
  : expr_app_do expr_op_do* -> ^(OP_LEFT expr_app_do expr_op_do*)
  | expr_op_do+ -> ^(SECTION expr_op_do+)
  ;

expr_op_do
  : EXPR_OP expr_app_do -> ^(OP_RIGHT EXPR_OP expr_app_do)
  ;

expr_app_do
  : expr_prim_do+ -> ^(APP expr_prim_do+)
  ;

bind_name
  : ID -> ^(IDENT ID)
  | EXPR_OP -> ^(IDENT EXPR_OP)
  ;
  
local_bind
  : bind_name maybe_has_type IS expr -> ^(BIND ID maybe_has_type expr)
  ;
  
local_binds 
  : local_bind (SEP local_bind)* SEP? -> local_bind+
  ;

expr_end 
  : WHERE^ local_binds END!
  | END -> ^(WHERE)
  ;

expr_prim_do
  : FN^ val_param* RARROW! expr expr_end
  | FN^ OF! fn_clause (SEP! fn_clause)* SEP!? expr_end
  | REC local_binds expr_end
  | CASE^ expr OF! case_clause (SEP! case_clause)* SEP!? expr_end
  | BEGIN! expr^ expr_end!
  | name^
  | INT^
  | FLOAT^
  | STRING^
  | CHAR^
  | TODO^
  ;

expr_prim
  : expr_prim_do^
  | DO^ do_stmt (SEP! do_stmt)* SEP!? expr_end
  | LET^ local_binds IN! expr expr_end
  | LPAREN! expr^ RPAREN!
  ;

fn_clause
  : pat_params RARROW expr -> ^(CLAUSE pat_params expr)
  ;

case_clause
  : pat_app RARROW expr -> ^(CLAUSE pat_app expr)
  ;

pat_params
  : pat_param+ -> ^(PARAMS pat_param+)
  ;

pat_param
  : pat -> ^(PARAM pat)
  ;

pat_app 
  : pat+ -> ^(APP pat+)
  ;

pat
  : name^
  | LPAREN! pat_app^ RPAREN!
  | simple_pat^
  ;

simple_pat 
  : INT^
  | STRING^
  | CHAR^
  ;
  
do_stmt 
  : LET^ local_binds END!
  | pat_app LARROW^ expr
  | expr_do^
  ;

val_params
  : val_param* -> ^(PARAMS val_param*)
  ;
  
val_param
  : ID -> ^(PARAM ID)
  | LPAREN ID has_type RPAREN -> ^(PARAM ID has_type)
  ;

maybe_has_type
  : has_type^
  | -> ^(HAS_TYPE)
  ;

has_type
  : HAS_TYPE^ type
  ;
  
type 
  : type_quant? type_core constraint* -> ^(TYPE type_core type_quant? constraint*)
  ;
  
type_quant 
  : FORALL^ ID+ DOT!
  ;
  
type_core 
  : type_app (RARROW type_app)* -> ^(FNTYPE type_app+)
  ;

type_app
  : type_prim+ -> ^(APP type_prim+)
  ;
  
type_prim 
  : AUTO^
  | name^
  | LPAREN! type_core^ RPAREN!
  | REC^ ID has_type (SEP! ID has_type)* SEP!? END!
  ;
  
constraint
  :  WITH^ type_core type_comp_op type_core
  ;
  
type_comp_op 
  : SUBTYPE^
  | SUPERTYPE^
  | HAS_TYPE^
  ;
  
type_params 
  : ID* -> ^(TYPE_PARAMS ID*)
  ;

name 
  : NAME -> ^(IDENT NAME)
  |	ID -> ^(IDENT ID)
  ;

ID  : ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*
    ;

NAME 
  : ID ('.' ID)*
  ;
    
EXPR_OP 
  : ('+'|'-'|'*'|'/'|'='|'<'|'>')+
  ;

INT : '0'..'9'+
    ;

FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;

COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {$channel=HIDDEN;}
    ;

STRING
    :  '"' ( ESC_SEQ | ~('\\'|'"') )* '"'
    ;

CHAR:  '\'' ( ESC_SEQ | ~('\''|'\\') ) '\''
    ;

fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F') ;

fragment
ESC_SEQ
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UNICODE_ESC
    |   OCTAL_ESC
    ;

fragment
OCTAL_ESC
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UNICODE_ESC
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;
