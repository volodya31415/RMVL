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
#define LIBMVL_VECTOR_CSTRING	101     /* C string is just like UINT8, except that the data is considered valid up to length or
first 0 byte */
#define LIBMVL_VECTOR_POSTAMBLE 1000

typedef unsigned long long LIBMVL_OFFSET64;

typedef struct {
	char signature[4];
	float endianness;
	unsigned int alignment;
	
	int reserved[13];
	} LIBMVL_PREAMBLE;

typedef struct {
	LIBMVL_OFFSET64 directory;
	int type;
	int reserved[13];
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
	long size;
	long free;
	LIBMVL_OFFSET64 *offset;
	char **tag;
	long *tag_length;
	
	/* Optional hash table */
	
	long *next_item;
	long *first_item;
	long hash_size;
	long hash_mult;
	} LIBMVL_NAMED_LIST;
	
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
#define LIBMVL_ERR_CORRUPT_POSTAMBLE	-11
#define LIBMVL_ERR_INVALID_ATTR_LIST	-12
#define LIBMVL_ERR_INVALID_OFFSET	-13
#define LIBMVL_ERR_INVALID_ATTR		-14
	
LIBMVL_CONTEXT *mvl_create_context(void);
void mvl_free_context(LIBMVL_CONTEXT *ctx);

#define LIBMVL_NO_METADATA 	0
#define LIBMVL_NULL_OFFSET 	0

LIBMVL_OFFSET64 mvl_write_vector(LIBMVL_CONTEXT *ctx, int type, long length, void *data, LIBMVL_OFFSET64 metadata);
LIBMVL_OFFSET64 mvl_write_concat_vectors(LIBMVL_CONTEXT *ctx, int type, long nvec, long *lengths, void **data, LIBMVL_OFFSET64 metadata);
/* Writes a single C string. In particular, this is handy for providing metadata tags */
/* length can be specified as -1 to be computed automatically */
LIBMVL_OFFSET64 mvl_write_string(LIBMVL_CONTEXT *ctx, long length, char *data, LIBMVL_OFFSET64 metadata);

void mvl_add_directory_entry(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 offset, char *tag);
void mvl_add_directory_entry_n(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 offset, char *tag, LIBMVL_OFFSET64 tag_size);
LIBMVL_OFFSET64 mvl_write_directory(LIBMVL_CONTEXT *ctx);

LIBMVL_NAMED_LIST *mvl_create_named_list(int size);
void mvl_free_named_list(LIBMVL_NAMED_LIST *L);
long mvl_add_list_entry(LIBMVL_NAMED_LIST *L, long tag_length, const char *tag, LIBMVL_OFFSET64 offset);
LIBMVL_OFFSET64 mvl_find_list_entry(LIBMVL_NAMED_LIST *L, long tag_length, const char *tag);
LIBMVL_OFFSET64 mvl_write_attributes_list(LIBMVL_CONTEXT *ctx, LIBMVL_NAMED_LIST *L);
/* This is meant to operate on memory mapped (or in-memory) files */
LIBMVL_NAMED_LIST *mvl_read_attributes_list(LIBMVL_CONTEXT *ctx, void *data, LIBMVL_OFFSET64 metadata_offset);

/* Convenience function that create a named list populated with necessary entries
 * It needs writable context to write attribute values */
LIBMVL_NAMED_LIST *mvl_create_R_attributes_list(LIBMVL_CONTEXT *ctx, char *R_class);

/* This function writes contents of named list and creates R-compatible metadata with entry names */
LIBMVL_OFFSET64 mvl_write_named_list(LIBMVL_CONTEXT *ctx, LIBMVL_NAMED_LIST *L);
/* This is meant to operate on memory mapped (or in-memory) files */
LIBMVL_NAMED_LIST *mvl_read_named_list(LIBMVL_CONTEXT *ctx, void *data, LIBMVL_OFFSET64 offset);

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
