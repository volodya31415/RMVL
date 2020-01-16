/* (c) Vladimir Dergachev 2019 */

#ifndef __LIBMVL_H__
#define __LIBMVL_H__

#include <stdio.h>

/* Mappable Vector Library - 
 * a structured file format which can be efficiently used 
 * after read-only memory mapping, can be appended while mapped, 
 * with versionable edits
 */

#define LIBMVL_SIGNATURE "MVL0"
#define LIBMVL_ENDIANNESS_FLAG 1.0

#define LIBMVL_VECTOR_UINT8	1
#define LIBMVL_VECTOR_INT32	2
#define LIBMVL_VECTOR_INT64	3
#define LIBMVL_VECTOR_FLOAT	4
#define LIBMVL_VECTOR_DOUBLE	5
#define LIBMVL_VECTOR_OFFSET64	100
#define LIBMVL_VECTOR_CSTRING	101

typedef unsigned long long LIBMVL_OFFSET64;

typedef struct {
	char signature[4];
	float endianness;
	unsigned int alignment;
	
	int reserved[13];
	} LIBMVL_PREAMBLE;

typedef struct {
	LIBMVL_OFFSET64 directory;
	int reserved[14];
	} LIBMVL_POSTAMBLE;
	
typedef struct {
	LIBMVL_OFFSET64 length;
	int type;
	int reserved[11];
	LIBMVL_OFFSET64 metadata;
	} LIBMVL_VECTOR_HEADER;
	
typedef struct {
	LIBMVL_VECTOR_HEADER header;
	union {
		unsigned char b[8];
		int i[2];
		long long i64[1];
		float f[2];
		double d[1];
		LIBMVL_OFFSET64 offset[1];
		} u;
	} LIBMVL_VECTOR;

typedef struct {
	LIBMVL_OFFSET64 offset;
	char *tag;
	} LIBMVL_DIRECTORY_ENTRY;
	
typedef struct {
	int alignment;
	int error;

	long dir_size;
	long dir_free;
	LIBMVL_DIRECTORY_ENTRY *directory;	
	LIBMVL_OFFSET64 directory_offset;
	
	FILE *f;
	
	
	LIBMVL_PREAMBLE tmp_preamble;
	LIBMVL_POSTAMBLE tmp_postamble;
	LIBMVL_VECTOR_HEADER tmp_vh;
	
	int abort_on_error;
	
	} LIBMVL_CONTEXT;
	
#define LIBMVL_ERR_FAIL_PREAMBLE	-1
#define LIBMVL_ERR_FAIL_POSTAMBLE	-2
#define LIBMVL_ERR_UNKNOWN_TYPE		-3
#define LIBMVL_ERR_FAIL_VECTOR		-4
#define LIBMVL_ERR_INCOMPLETE_WRITE	-5
#define LIBMVL_ERR_INVALID_SIGNATURE 	-6
#define	LIBMVL_ERR_WRONG_ENDIANNESS	-7
#define LIBMVL_ERR_EMPTY_DIRECTORY	-8
#define LIBMVL_ERR_INVALID_DIRECTORY	-9
#define LIBMVL_ERR_FTELL		-10

LIBMVL_CONTEXT *mvl_create_context(void);
void mvl_free_context(LIBMVL_CONTEXT *ctx);

#define LIBMVL_NO_METADATA 	0

LIBMVL_OFFSET64 mvl_write_vector(LIBMVL_CONTEXT *ctx, int type, long length, void *data, LIBMVL_OFFSET64 metadata);

void mvl_add_directory_entry(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 offset, char *tag);
void mvl_add_directory_entry_n(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 offset, char *tag, LIBMVL_OFFSET64 tag_size);
LIBMVL_OFFSET64 mvl_write_directory(LIBMVL_CONTEXT *ctx);
	
void mvl_open(LIBMVL_CONTEXT *ctx, FILE *f);
void mvl_close(LIBMVL_CONTEXT *ctx);
void mvl_write_preamble(LIBMVL_CONTEXT *ctx);
void mvl_write_postamble(LIBMVL_CONTEXT *ctx);

#define mvl_vector_type(data)   (((LIBMVL_VECTOR_HEADER *)(data))->type)
#define mvl_vector_length(data)   (((LIBMVL_VECTOR_HEADER *)(data))->length)
#define mvl_vector_data(data)   ((((LIBMVL_VECTOR *)(data))->u))
#define mvl_vector_metadata_offset(data)   ((((LIBMVL_VECTOR_HEADER *)(data))->metadata))

LIBMVL_OFFSET64 mvl_find_directory_entry(LIBMVL_CONTEXT *ctx, char *tag);

/* This initializes context to use in-memory image of given length starting at data
 * the image could have been loaded via fread, or memory mapped
 */
void mvl_load_image(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 length, void *data);

#endif