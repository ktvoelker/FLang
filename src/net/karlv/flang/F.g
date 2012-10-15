grammar F;

options {
  language = Java;
  output = AST;
}

@header {
  package net.karlv.flang;
}

@lexer::header {
  package net.karlv.flang;
}

start
  : file EOF
  ;

file
  : module_header decl*
  | sig_header sig_decl*
  ;

module_header 
  : MODULE ID type_params (':' type)?
  ;

sig_header 
  : 'sig' ID type_params
  ;

module_app
  : module_prim+
  ;

module_prim
  : name
  | '(' module_app ')'
  ;
  
decl 
  : OPEN module_app (('except' | 'only') (ID | EXPR_OP)+)?
  | 'val' (ID | EXPR_OP) (':' type)? 'is' expr
  | 'data' ('open' | 'closed')? ID type_params ('<:' type) 'is' type
  | 'type' ID type_params 'is' type
  | module_header 'is' (module_app | decl* END)
  | sig_header 'is' sig_decl* END
  ;
  
sig_decl 
  : 'val' (ID | EXPR_OP) ':' type
  | 'type' ID type_params (type_comp_op type)?
  | MODULE ID type_params ':' type
  ;

expr
  : EXPR_OP? expr_prim+ (EXPR_OP expr_prim+)*
  ;
  
local_bind
  : ID '=' expr
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
  | '(' ID ':' type ')'
  ;
  
type 
  : type_quant? type_core constraint*
  ;
  
type_quant 
  : 'forall' ID+ '.'
  ;
  
type_core 
  : type_prim+ ('->' type_prim+)*
  ;
  
type_prim 
  : '*'
  | name
  | '(' type_core ')'
  | 'rec' ID ':' type (';' ID ':' type)* ';'? END
  ;
  
constraint
  :  WITH type_core type_comp_op type_core
  ;
  
type_comp_op 
  : '<:'
  | ':>'
  | ':'
  ;
  
type_params 
  : ID*
  ;

name 
  : NAME
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
