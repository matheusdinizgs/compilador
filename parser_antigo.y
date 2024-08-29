%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "ast.h"
#include "semantic.h"
#include "tacs.h"

int yyerror(char *message);
int yylex();
extern int getLineNumber();    

AST *root;

%}

%union
{
    AST *ast;
    HASH_NODE *symbol;
}

%token KW_CHAR      
%token KW_INT       
%token KW_FLOAT     
%token KW_BOOL      
%token KW_IF        
%token KW_ELSE      
%token KW_WHILE     
%token KW_READ      
%token KW_PRINT     
%token KW_RETURN    
%token OPERATOR_LE  
%token OPERATOR_GE  
%token OPERATOR_EQ  
%token OPERATOR_DIF 
%token<symbol> TK_IDENTIFIER
%token<symbol> LIT_CHAR     
%token<symbol> LIT_INT
%token<symbol> LIT_REAL     
%token<symbol> LIT_FALSE    
%token<symbol> LIT_TRUE     
%token<symbol> LIT_STRING   

%token TOKEN_ERROR

%left '.' '|' '&' '~'
%left '<' '>' OPERATOR_EQ OPERATOR_DIF OPERATOR_LE OPERATOR_GE
%left '+' '-'
%left '*' '/'


%type<ast> programa
%type<ast> decl
%type<ast> dec
%type<ast> type
%type<ast> literal
%type<ast> init_vector
%type<ast> vector_elements
%type<ast> l_param
%type<ast> param_end
%type<ast> param
%type<ast> body
%type<ast> cmd_list
%type<ast> cmd_list_end
%type<ast> cmd
%type<ast> expr
%type<ast> l_args
%type<ast> l_args_end
%type<ast> print
%type<ast> element_print

%start programa

%%

programa: decl          {root = $$; astPrint(root, 0); fprintf(stderr,"\n"); 
                         SemanticErrors = checkSemantics(root);
                         tacPrintBackwards(generateCode(root, 0));}
    ;

decl : dec decl         { $$ = astCreate(AST_DECL, 0, $1, $2, 0, 0);}
    |                   { $$ = 0;}
    ;

dec: type TK_IDENTIFIER ':' literal ';'                     { $$ = astCreate(AST_DECVAR, $2, $1, $4, 0, 0);}
    | type TK_IDENTIFIER '[' LIT_INT ']' init_vector ';'    { $$ = astCreate(AST_DECVET, $2, $1, astCreate(AST_SYMBOL, $4, 0, 0, 0, 0), $6, 0);}
    | type TK_IDENTIFIER '(' l_param ')' body               { $$ = astCreate(AST_DECFUNC, $2, $1, $4, $6, 0);}
    ;

type: KW_CHAR       { $$ = astCreate(AST_KWCHAR, 0, 0, 0, 0, 0);}
    | KW_BOOL       { $$ = astCreate(AST_KWBOOL, 0, 0, 0, 0, 0);}
    | KW_INT        { $$ = astCreate(AST_KWINT, 0, 0, 0, 0, 0);}
    | KW_FLOAT      { $$ = astCreate(AST_KWFLOAT, 0, 0, 0, 0, 0);}
    ;

literal: LIT_INT        { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0); }
       | LIT_CHAR       { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0); }
       | LIT_TRUE       { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0); }
       | LIT_FALSE      { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0); }
       | LIT_REAL       { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0); }
       ;

init_vector :                               { $$ = 0;}
            | ':' literal vector_elements   { $$ = astCreate(AST_INIT_VETOR, 0, $2, $3, 0, 0);}
            ;

vector_elements :                           { $$ = 0;}
                | literal vector_elements   { $$ = astCreate(AST_ELEM_VETOR, 0, $1, $2, 0, 0);}
                ;

l_param :                       { $$ = 0;}
        | param param_end       { $$ = astCreate(AST_INIT_PARAM, 0, $1, $2, 0, 0);}
        ;

param_end:                      { $$ = 0;}
		| ',' param param_end   { $$ = astCreate(AST_L_PARAM, 0, $2, $3, 0, 0);}
        ;

param : type TK_IDENTIFIER      { $$ = astCreate(AST_PARAM, $2, $1, 0, 0, 0);}
      ;

body: '{' cmd_list '}'          { $$ = astCreate(AST_BODY, 0, $2, 0, 0, 0);}
    ;

cmd_list: cmd cmd_list_end      { $$ = astCreate(AST_INIT_L_CMD, 0, $1, $2, 0, 0);}    
	;

cmd_list_end: ';' cmd_list      { $$ = astCreate(AST_L_CMD, 0, $2, 0, 0, 0);}
    |                           { $$ = 0;}
    ;

cmd:                                        { $$ = 0;}
    | TK_IDENTIFIER '=' expr                { $$ = astCreate(AST_ATRIBU, $1, $3, 0, 0, 0);}
    | KW_IF '(' expr ')' cmd                { $$ = astCreate(AST_IF, 0, $3, $5, 0, 0);}
    | KW_IF '(' expr ')' cmd KW_ELSE cmd    { $$ = astCreate(AST_IF_ELSE, 0, $3, $5, $7, 0);}
    | TK_IDENTIFIER '[' expr ']' '=' expr   { $$ = astCreate(AST_ATRIBU_VETOR, $1, $3, $6, 0, 0);}
    | KW_READ type TK_IDENTIFIER            { $$ = astCreate(AST_READ, $3, $2, 0, 0, 0);}
    | KW_PRINT print                        { $$ = astCreate(AST_PRINT, 0, $2, 0, 0, 0);}
    | KW_RETURN expr                        { $$ = astCreate(AST_RETURN, 0, $2, 0, 0, 0);}
    | KW_WHILE '(' expr ')' cmd             { $$ = astCreate(AST_WHILE, 0, $3, $5, 0, 0);}
    | body cmd                              { $$ = astCreate(AST_BODY_CMD, 0, $1, $2, 0, 0);}
    ;

expr: literal                       { $$ = $1;}
    | TK_IDENTIFIER                 { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0); }
    | TK_IDENTIFIER '[' expr ']'    { $$ = astCreate(AST_VETOR, $1, $3, 0, 0, 0);}
    | TK_IDENTIFIER '(' l_args ')'  { $$ = astCreate(AST_FUNC, $1, $3, 0, 0, 0);}
    | expr '+' expr                 { $$ = astCreate(AST_ADD, 0, $1, $3, 0, 0);}
    | expr '-' expr                 { $$ = astCreate(AST_SUB, 0, $1, $3, 0, 0);}
    | expr '*' expr                 { $$ = astCreate(AST_MUL, 0, $1, $3, 0, 0);}
    | expr '/' expr                 { $$ = astCreate(AST_DIV, 0, $1, $3, 0, 0);}
    | expr '<' expr                 { $$ = astCreate(AST_MENOR, 0, $1, $3, 0, 0);}
    | expr '>' expr                 { $$ = astCreate(AST_MAIOR, 0, $1, $3, 0, 0);}
    | expr OPERATOR_EQ expr         { $$ = astCreate(AST_IGUAL, 0, $1, $3, 0, 0);}
    | expr OPERATOR_DIF expr        { $$ = astCreate(AST_DIF, 0, $1, $3, 0, 0);}
    | expr OPERATOR_GE expr         { $$ = astCreate(AST_MAIOR_IGUAL, 0, $1, $3, 0, 0);}
    | expr OPERATOR_LE expr         { $$ = astCreate(AST_MENOR_IGUAL, 0, $1, $3, 0, 0);}
    | expr '|' expr                 { $$ = astCreate(AST_OR, 0, $1, $3, 0, 0); }
    | expr '&' expr                 { $$ = astCreate(AST_AND, 0, $1, $3, 0, 0); }
    | '~' expr                      { $$ = astCreate(AST_NOT, 0, $2, 0, 0, 0); }
    | '(' expr ')'                  { $$ = astCreate(AST_PAREN, 0, $2, 0, 0, 0);}
    ;

l_args:                             { $$ = 0;}
    | expr l_args_end               { $$ = astCreate(AST_INIT_L_ARG, 0, $1, $2, 0, 0);}
    ;

l_args_end:                         { $$ = 0;}
    | ',' expr l_args_end           { $$ = astCreate(AST_L_ARG, 0, $2, $3, 0, 0);}
    ;

print: element_print                { $$ = astCreate(AST_INIT_L_PRINT, 0, $1, 0, 0, 0);}
    ;

element_print: LIT_STRING           { $$ = astCreate(AST_SYMBOL, $1, 0, 0, 0, 0);}
    | type expr                     { $$ = astCreate(AST_TYPE_PRINT, 0, $1, $2, 0, 0);}
    ;

%%

void checkSemantic(){
  if(SemanticErrors > 0){
    fprintf(stderr, "%d Semantic Errors in Total.\n", SemanticErrors);
    exit(4);
  }
}

int yyerror(char *err)
{
    fprintf(stderr, "Syntax error in line %d.\n", getLineNumber());
    hashPrint();
    exit(3);
}

AST* getAST(){
	return root;
}
