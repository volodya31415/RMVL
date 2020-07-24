#ifndef __OPERATIONS_H__
#define __OPERATIONS_H__



#define OPERATION_CONST_INT	1
#define OPERATION_CONST_DOUBLE	2
#define OPERATION_CONST_STRING	3

#define OPERATION_COLUMN	5
#define OPERATION_NEG		6
#define OPERATION_NOT		7
#define OPERATION_FUNC		8

#define OPERATION_ADD		10
#define OPERATION_SUB		11
#define OPERATION_MUL		12
#define OPERATION_DIV		13
#define OPERATION_REM		14
#define OPERATION_LT		15
#define OPERATION_GT		16
#define OPERATION_LE		17
#define OPERATION_GE		18
#define OPERATION_EQ		19
#define OPERATION_NEQ		20
#define OPERATION_AND		21
#define OPERATION_OR		22
#define OPERATION_POW		23

#define OPERATION_COLUMNS	1000
#define OPERATION_TABLES	1001
#define OPERATION_TABLES_JOIN_USING	1002
#define OPERATION_TABLES_JOIN_ON	1003

#define OPERATION_SELECT	1010


typedef struct S_OPERATION {
	int type;
	char *table;
	char *name;
	int ival;
	double dval;
	struct S_OPERATION *op[3];
	} OPERATION;

OPERATION *new_op(void);
OPERATION *shallow_op_dup(OPERATION *op);
void dump_operations(OPERATION *op);
	
#endif
