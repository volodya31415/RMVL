%define api.prefix {sql_yy}
%define api.pure
%define parse.trace true
%defines
%parse-param {OPERATION **sql_code}

%{
#include "sql_operations.h"
%}

%union 
	{
	int	ival;
	double  dval;
	char *str;
	OPERATION *op;
	}

%token <str> NAME
%token <str> STRING

%token <ival> SELECT
%token <ival> FROM
%token <ival> WHERE
%token <ival> JOIN
%token <ival> AS

%token <ival> OP
%token <ival> INUMBER
%token <dval> DNUMBER

%token <ival> COMMA
%token <ival> DOT
%token <ival> LPAREN
%token <ival> RPAREN

%type <op> term
%type <op> expression
%type <op> columns
%type <op> tables
%type <op> sql_select

%left '<' '=' '>' ">=" "<=" "!="
%left '&' '|' 

%left '-' '+'
%left '*' '/' '%'
%precedence '!'
%right '^'

%{
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;

#include "sql_parser.h"
#include "sql_scanner.h"
%}

%%

input: sql_select   {*sql_code=$1;};

sql_select: SELECT columns FROM tables   {$$=new_op(); $$->type=OPERATION_SELECT; $$->op[0]=$2; $$->op[1]=$4; $$->op[2]=NULL;}
	| SELECT columns FROM tables WHERE expression  {$$=new_op(); $$->type=OPERATION_SELECT; $$->op[0]=$2; $$->op[1]=$4; $$->op[2]=$6;};
	
columns:  expression {$$=new_op(); $$->type=OPERATION_COLUMNS; $$->name=strdup(""); $$->op[1]=$1; $$->op[0]=NULL;}
	| expression AS NAME {$$=new_op(); $$->type=OPERATION_COLUMNS; $$->name=$3; $$->op[1]=$1; $$->op[0]=NULL;}
	| columns ',' expression {$$=new_op(); $$->type=OPERATION_COLUMNS; $$->name=strdup(""); $$->op[0]=$1; $$->op[1]=$3; }
	| columns ',' expression AS NAME {$$=new_op(); $$->type=OPERATION_COLUMNS; $$->name=$5; $$->op[0]=$1; $$->op[1]=$3; };
	
tables: NAME {$$=new_op(); $$->type=OPERATION_TABLES; $$->name=$1; $$->op[0]=NULL; }
	| tables JOIN NAME {$$=new_op(); $$->type=OPERATION_TABLES; $$->name=$3; $$->op[0]=$1;};
	
expression: term		{$$=$1;}
	| '-' term {$$=new_op(); $$->type=OPERATION_NEG; $$->op[0]=$2; }
	| '!' expression {$$=new_op(); $$->type=OPERATION_NOT; $$->op[0]=$2; }
	| expression '+' term 	{$$=new_op(); $$->type=OPERATION_ADD; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '*' term 	{$$=new_op(); $$->type=OPERATION_MUL; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '/' term 	{$$=new_op(); $$->type=OPERATION_DIV; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '-' term 	{$$=new_op(); $$->type=OPERATION_SUB; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '^' term 	{$$=new_op(); $$->type=OPERATION_POW; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '&' term 	{$$=new_op(); $$->type=OPERATION_AND; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '|' term 	{$$=new_op(); $$->type=OPERATION_OR; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '>' term 	{$$=new_op(); $$->type=OPERATION_GT; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '<' term 	{$$=new_op(); $$->type=OPERATION_LT; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression '=' term 	{$$=new_op(); $$->type=OPERATION_EQ; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression ">=" term 	{$$=new_op(); $$->type=OPERATION_GE; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression "<=" term 	{$$=new_op(); $$->type=OPERATION_LE; $$->op[0]=$1; $$->op[1]=$3;} 
	| expression "!=" term 	{$$=new_op(); $$->type=OPERATION_NEQ; $$->op[0]=$1; $$->op[1]=$3;} ;

term:	NAME {$$=new_op(); $$->type=OPERATION_COLUMN; $$->table=strdup(""); $$->name=$1; }
	| NAME '.' NAME {$$=new_op();  $$->type=OPERATION_COLUMN; $$->table=$1; $$->name=$3; }
	| INUMBER {$$=new_op(); $$->type=OPERATION_CONST_INT; $$->ival=$1; }
	| DNUMBER {$$=new_op(); $$->type=OPERATION_CONST_DOUBLE; $$->dval=$1; }
	| STRING  {$$=new_op(); $$->type=OPERATION_CONST_STRING; $$->name=$1; }
	| '(' expression ')' {$$=$2;};
	| NAME '(' expression ')' {$$=new_op(); $$->type=OPERATION_FUNC; $$->op[0]=$3; $$->name=$1;};
	
%%

