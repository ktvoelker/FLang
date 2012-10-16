grammar F;

options {
  language = Java;
  output = AST;
}

tokens {
  FLAGS;
  DECLS;
  TYPE;
  TYPE_PARAMS;
  PARENT;
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

is
  : 'is'!
  ;

file
  : module_header^ decls
  | sig_header^ sig_decls
  ;

module_header 
  : MODULE^ ID type_params has_type?
  ;

sig_header 
  : 'sig'^ ID type_params
  ;

module_app
  : module_prim+
  ;

module_prim
  : name
  | '(' module_app ')'
  ;
 
decls
  : decl* -> ^(DECLS decl*)
  ;
  
decl 
  : OPEN^ module_app (('except' | 'only') (ID | EXPR_OP)+)?
  | 'val'^ id=(ID | EXPR_OP) has_type? is body=expr
  | 'data'^ data_flags ID type_params parent_type is type
  | 'type'^ ID type_params is type
  | module_header^ is (module_app | decls END)
  | sig_header^ is sig_decls END
  ;

data_flags
  : 'open' -> ^(FLAGS 'open')
  | 'closed' -> ^(FLAGS 'closed')
  | -> ^(FLAGS)
  ;
  
parent_type
  : '<:' type -> ^(PARENT type)
  | -> ^(PARENT)
  ;

sig_decls
  : sig_decl* -> ^(DECLS sig_decl*)
  ;
  
sig_decl 
  : 'val' (ID | EXPR_OP) has_type
  | 'type' ID type_params (type_comp_op type)?
  | MODULE ID type_params has_type
  ;

expr
  : EXPR_OP? expr_prim+ (EXPR_OP expr_prim+)*
  ;
  
local_bind
  : ID has_type? '=' expr
  ;
  
local_binds 
  : local_bind (';' local_bind)* ';'?
  ;

expr_end 
  : (WHERE local_binds)? END
  ;

expr_prim
  : FN val_params '->' expr expr_end
  | FN OF (pats '->' expr ';') expr_end
  | REC local_binds expr_end
  | CASE expr OF (pat_app '->' expr ';') expr_end
  | LPAREN expr RPAREN
  | BEGIN expr expr_end
  | DO do_stmt (';' do_stmt)* ';'? expr_end
  | LET local_binds IN expr expr_end
  | name
  | INT
  | FLOAT
  | STRING
  | CHAR
  | TODO
  ;
  
pats 
  : pat+
  ;

pat_app 
  : pat+
  ;

pat : name
  | '(' pat_app ')'
  | simple_pat
  ;

simple_pat 
  : INT
  | STRING
  | CHAR
  ;
  
do_stmt 
  : LET pat_app '=' expr
  | pat_app '<-' expr
  // |  expr
  ;
  
val_params 
  : ID
  | '(' ID has_type ')'
  ;

has_type
  : ':' type^
  ;
  
type 
  : type_quant? type_core constraint* -> ^(TYPE type_core type_quant? constraint*)
  ;
  
type_quant 
  : 'forall'^ ID+ '.'!
  ;
  
type_core 
  : type_prim+ ('->' type_prim+)*
  ;
  
type_prim 
  : '*'
  | name
  | '(' type_core ')'
  | 'rec' ID has_type (';' ID ':' type)* ';'? END
  ;
  
constraint
  :  WITH^ type_core type_comp_op type_core
  ;
  
type_comp_op 
  : '<:'
  | ':>'
  | ':'
  ;
  
type_params 
  : ID* -> ^(TYPE_PARAMS ID*)
  ;

name 
  : NAME
  |	ID
  ;

todo 
  : 'todo'
  ;

LET : 'let'
  ;
  
FN  :  'fn'
  ;
  
CASE 
  : 'case'
  ;
  
REC : 'rec'
  ;
  
BEGIN 
  : 'begin'
  ;
  
DO  : 'do'
  ;
  
OPEN 
  : 'open'
  ;
  
TODO 
  : '?'
  ;
  
LPAREN 
  : '('
  ;
  
RPAREN 
  : ')'
  ;
  
WHERE 
  : 'where'
  ;

END : 'end'
  ;
  
IN  : 'in'
  ;
  
OF  : 'of'
  ;
  
MODULE 
  : 'module'
  ;
  
WITH 
  : 'with'
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
