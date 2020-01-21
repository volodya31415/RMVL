%define api.prefix {sql_yy}
%define api.pure
%define parse.trace true
%defines

%union 
	{
	int	ival;
	double  dval;
	char const *str;
	}

%token <string> NAME

%token <ival> SELECT
%token <ival> FROM
%token <ival> WHERE
%token <ival> JOIN

%token <ival> OP
%token <ival> INUMBER
%token <dval> DNUMBER

%token <ival> COMMA
%token <ival> DOT
%token <ival> LPAREN
%token <ival> RPAREN

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

sql_select: SELECT columns FROM tables   {}
	| SELECT columns FROM tables WHERE expression  {};
	
columns:  expression {}
	| columns COMMA expression {};
	
tables: NAME {}
	| tables JOIN NAME {};
	
expression: term		{}
	| expression '+' term 	{} 
	| expression '*' term 	{} 
	| expression '/' term 	{} 
	| expression '-' term 	{} 
	| expression '^' term 	{} 
	| expression '&' term 	{} 
	| expression '|' term 	{} 
	| expression '>' term 	{} 
	| expression '<' term 	{} 
	| expression '=' term 	{} 
	| expression ">=" term 	{} 
	| expression "<=" term 	{} 
	| expression "!=" term 	{} ;

term:	NAME {}
	| NAME '.' NAME {}
	| INUMBER {}
	| DNUMBER {}
	| '(' expression ')' {};
	| NAME '(' expression ')' {};
	
%%

