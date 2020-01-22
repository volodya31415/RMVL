%option noyywrap
%option bison-bridge
%option prefix="sql_yy"
%option header-file="sql_scanner.h"

%{

#include "sql_operations.h"
#include "sql_parser.h"

#define YYSTYPE SQL_YYSTYPE

%}

%%

[Ss][Ee][Ll][Ee][Cc][Tt][ \t\n]		{ return SELECT; }

[ \t\n][Jj][Oo][Ii][Nn][ \t\n]		{ return JOIN; }

[ \t\n][Ff][Rr][Oo][Mm][ \t\n]		{ return FROM; }

[ \t\n][Ww][Hh][Ee][Rr][Ee][ \t\n]		{ return WHERE; }

[ \t\n][Aa][Ss][ \t\n]			{ return AS; }

[-+*&|%=><.,()^]			{ yylval->ival=yytext[0]; return yytext[0]; }

'[^`]*'					{ yylval->str=strndup(&(yytext[1]), yyleng-2); return NAME; }

"[^`]*"					{ yylval->str=strndup(&(yytext[1]), yyleng-2); return STRING; }

[ \t\n]+				{}

[^-+*&|%=><.,()^ \t\n]+				{ yylval->str=strndup(yytext, yyleng); printf("NAME=\"%s\"\n", yylval->str); return NAME; }

%%



void compile_sql(char *sql_line, int n, OPERATION **sql_code)
{
YY_BUFFER_STATE bst;
if(n<0)
	bst=yy_scan_string(sql_line);
	else
	bst=yy_scan_bytes(sql_line, n);
sql_yyparse(sql_code);
yy_delete_buffer(bst);
}
