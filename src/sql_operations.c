#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "sql_operations.h"

static void *do_malloc(long a, long b)
{
void *r;
int i=0;
if(a<1)a=1;
if(b<1)b=1;
r=malloc(a*b);
while(r==NULL){
	fprintf(stderr,"libMVL: Could not allocate %ld chunks of %ld bytes each (%ld bytes total)\n",a,b,a*b);
//	if(i>args_info.memory_allocation_retries_arg)exit(-1);
	sleep(10);
	r=malloc(a*b);
	i++;
	}
//if(a*b>10e6)madvise(r, a*b, MADV_HUGEPAGE);
return r;
}

OPERATION *new_op(void)
{
OPERATION *r;
r=do_malloc(1, sizeof(*r));
memset(r, 0, sizeof(*r));
return(r);
}

OPERATION *shallow_op_dup(OPERATION *op)
{
OPERATION *r;
r=do_malloc(1, sizeof(*r));
memcpy(r, op, sizeof(*r));
return(r);
}

static char *type2str(int t)
{
switch(t) {
	case OPERATION_CONST_INT: return("INT");
	case OPERATION_CONST_DOUBLE: return("DOUBLE");
	case OPERATION_CONST_STRING: return("STRING");
		
	case OPERATION_COLUMN: return("COLUMN");
	case OPERATION_NEG: return("-");
	case OPERATION_NOT: return("!");
	case OPERATION_FUNC: return("FUNC");
		
	case OPERATION_ADD: return("+");
	case OPERATION_SUB: return("-");
	case OPERATION_MUL: return("*");
	case OPERATION_DIV: return("/");
	case OPERATION_REM: return("%");
	case OPERATION_LT:  return("<");
	case OPERATION_GT:  return(">");
	case OPERATION_LE:  return("<=");
	case OPERATION_GE:  return(">=");
	case OPERATION_EQ:  return("==");
	case OPERATION_NEQ: return("!=");
	case OPERATION_AND: return("&");
	case OPERATION_OR:  return("|");
	case OPERATION_POW: return("^");

	case OPERATION_COLUMNS: return("COLUMNS");
	case OPERATION_TABLES: return("TABLES");
	case OPERATION_SELECT:	return("SELECT");
	
	default: return("UKNOWN");
	}
}

static int arity(int t)
{
switch(t) {
	case OPERATION_CONST_INT: 
	case OPERATION_CONST_DOUBLE: 
	case OPERATION_CONST_STRING: 
		
	case OPERATION_COLUMN: 
		return -t;
	
	case OPERATION_NEG: 
	case OPERATION_NOT: 
	case OPERATION_FUNC:
		return 1;
		
	case OPERATION_ADD: 
	case OPERATION_SUB: 
	case OPERATION_MUL: 
	case OPERATION_DIV: 
	case OPERATION_REM: 
	case OPERATION_LT:  
	case OPERATION_GT:  
	case OPERATION_LE:  
	case OPERATION_GE:  
	case OPERATION_EQ:  
	case OPERATION_NEQ: 
	case OPERATION_AND: 
	case OPERATION_OR:  
	case OPERATION_POW: 
		return(2);

	case OPERATION_COLUMNS:
	case OPERATION_TABLES: 
	case OPERATION_SELECT:	
		return -t;
	
	default: return(0);
	}
return(0);
}

void dump_operations(OPERATION *op)
{
int a;
int i;
if(op==NULL) {
	printf("(NULL)");
	return;
	}
	
printf("(%s ", type2str(op->type));
a=arity(op->type);
if(a>0) {
	for(i=0;i<a;i++)dump_operations(op->op[i]);
	} else {
	switch(op->type) {
		case OPERATION_CONST_INT: 
			printf(" %d", op->ival);
			break;
		case OPERATION_CONST_DOUBLE: 
			printf(" %g", op->dval);
			break;
		case OPERATION_CONST_STRING: 
			printf(" \"%s\"", op->name);
			break;
		case OPERATION_COLUMN: 
			printf(" \"%s.%s\"", op->table, op->name);
			break;
		case OPERATION_COLUMNS:
			if(op->op[0]!=NULL) {
				dump_operations(op->op[0]);
				printf(", ");
				dump_operations(op->op[1]);
				printf(" AS '%s'", op->name);
				} else {
				dump_operations(op->op[1]);
				printf(" AS '%s'", op->name);
				}
			break;
		case OPERATION_TABLES: 
			if(op->op[0]!=NULL) {
				dump_operations(op->op[0]);
				printf(" JOIN %s", op->name);
				} else {
				printf("%s", op->name);
				}
			break;
		case OPERATION_SELECT:	
			dump_operations(op->op[0]);
			printf(" FROM ");
			dump_operations(op->op[1]);
			printf(" WHERE ");
			dump_operations(op->op[2]);
			break;
		default:
			printf("UKNOWN_OP[%d-->%d]", op->type, a);
			break;
		}
	}
printf(")\n");
	
}
