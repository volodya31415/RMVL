
/* (c) Vladimir Dergachev 2019 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libMVL.h"

void *do_malloc(long a, long b)
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

LIBMVL_CONTEXT *mvl_create_context(void)
{
LIBMVL_CONTEXT *ctx;
//ctx=calloc(1, sizeof(*ctx));
ctx=do_malloc(1, sizeof(*ctx));
if(ctx==NULL)return(ctx);

ctx->error=0;
ctx->abort_on_error=1;
ctx->alignment=32;

ctx->dir_size=100;
ctx->dir_free=0;

ctx->directory=do_malloc(ctx->dir_size, sizeof(*ctx->directory));
ctx->directory_offset=-1;

return(ctx);
}

void mvl_free_context(LIBMVL_CONTEXT *ctx)
{
free(ctx);
}

void mvl_set_error(LIBMVL_CONTEXT *ctx, int error)
{
ctx->error=error;
if(ctx->abort_on_error) {
	fprintf(stderr, "*** ERROR: libMVL code %d\n", error);
	exit(-1);
	}
}

void mvl_write(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 length, void *data)
{
LIBMVL_OFFSET64 n;
n=fwrite(data, 1, length, ctx->f);
if(n<length)mvl_set_error(ctx, LIBMVL_ERR_INCOMPLETE_WRITE);
}

void mvl_write_preamble(LIBMVL_CONTEXT *ctx)
{
int n;
memset(&(ctx->tmp_preamble), 0, sizeof(ctx->tmp_preamble));
memcpy(ctx->tmp_preamble.signature, LIBMVL_SIGNATURE, 4);
ctx->tmp_preamble.endianness=LIBMVL_ENDIANNESS_FLAG;
ctx->tmp_preamble.alignment=ctx->alignment;
mvl_write(ctx, sizeof(ctx->tmp_preamble), &ctx->tmp_preamble);
}

void mvl_write_postamble(LIBMVL_CONTEXT *ctx)
{
int n;
memset(&(ctx->tmp_postamble), 0, sizeof(ctx->tmp_postamble));
ctx->tmp_postamble.directory=ctx->directory_offset;
mvl_write(ctx, sizeof(ctx->tmp_postamble), &ctx->tmp_postamble);
}

LIBMVL_OFFSET64 mvl_write_vector(LIBMVL_CONTEXT *ctx, int type, long length, void *data, LIBMVL_OFFSET64 metadata)
{
long byte_length;
int padding;
unsigned char *zeros;
LIBMVL_OFFSET64 offset;

memset(&(ctx->tmp_vh), 0, sizeof(ctx->tmp_vh));

switch(type) {
	case LIBMVL_VECTOR_CSTRING:
	case LIBMVL_VECTOR_UINT8:
		byte_length=length;
		break;
	case LIBMVL_VECTOR_INT32:
	case LIBMVL_VECTOR_FLOAT:
		byte_length=length*4;
		break;
	case LIBMVL_VECTOR_INT64:
	case LIBMVL_VECTOR_DOUBLE:
	case LIBMVL_VECTOR_OFFSET64:
		byte_length=length*8;
		break;
	default:
		mvl_set_error(ctx, LIBMVL_ERR_UNKNOWN_TYPE);
		return;
	}
padding=ctx->alignment-((byte_length+sizeof(ctx->tmp_vh)) & (ctx->alignment-1));
padding=padding & (ctx->alignment-1);

ctx->tmp_vh.length=length;
ctx->tmp_vh.type=type;
ctx->tmp_vh.metadata=metadata;

offset=ftello(ctx->f);

if((long long)offset<0) {
	perror("mvl_write_vector");
	mvl_set_error(ctx, LIBMVL_ERR_FTELL);
	}

mvl_write(ctx, sizeof(ctx->tmp_vh), &ctx->tmp_vh);
mvl_write(ctx, byte_length, data);

if(padding>0) {
	zeros=alloca(padding);
	memset(zeros, 0, padding);
	mvl_write(ctx, padding, zeros);
	}

return(offset);
}

void mvl_add_directory_entry(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 offset, char *tag)
{
LIBMVL_DIRECTORY_ENTRY *p;
if(ctx->dir_free>=ctx->dir_size) {
	ctx->dir_size+=ctx->dir_size+10;
	
	p=do_malloc(ctx->dir_size, sizeof(*p));
	if(ctx->dir_free>0)memcpy(p, ctx->directory, ctx->dir_free*sizeof(*p));
	free(ctx->directory);
	ctx->directory=p;
	}
ctx->directory[ctx->dir_free].offset=offset;
ctx->directory[ctx->dir_free].tag=strdup(tag);
ctx->dir_free++;
}

void mvl_add_directory_entry_n(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 offset, char *tag, LIBMVL_OFFSET64 tag_size)
{
LIBMVL_DIRECTORY_ENTRY *p;
if(ctx->dir_free>=ctx->dir_size) {
	ctx->dir_size+=ctx->dir_size+10;
	
	p=do_malloc(ctx->dir_size, sizeof(*p));
	if(ctx->dir_free>0)memcpy(p, ctx->directory, ctx->dir_free*sizeof(*p));
	free(ctx->directory);
	ctx->directory=p;
	}
ctx->directory[ctx->dir_free].offset=offset;
ctx->directory[ctx->dir_free].tag=strndup(tag, tag_size);
ctx->dir_free++;
}

LIBMVL_OFFSET64 mvl_write_directory(LIBMVL_CONTEXT *ctx)
{
LIBMVL_OFFSET64 *p;
LIBMVL_OFFSET64 offset;
int i;


if(ctx->dir_free<1) {
	mvl_set_error(ctx, LIBMVL_ERR_EMPTY_DIRECTORY);
	return(0);
	}

p=do_malloc(ctx->dir_free*2, sizeof(*p));
for(i=0;i<ctx->dir_free;i++) {
	p[i]=mvl_write_vector(ctx, LIBMVL_VECTOR_UINT8,  strlen(ctx->directory[i].tag), ctx->directory[i].tag, LIBMVL_NO_METADATA);
	p[i+ctx->dir_free]=ctx->directory[i].offset;
	}

	
offset=ftello(ctx->f);

if((long long)offset<0) {
	perror("mvl_write_directory");
	mvl_set_error(ctx, LIBMVL_ERR_FTELL);
	}

mvl_write_vector(ctx, LIBMVL_VECTOR_OFFSET64, 2*ctx->dir_free, p, LIBMVL_NO_METADATA);
	
ctx->dir_free=0;

ctx->directory_offset=offset;
return(offset);
}

void mvl_open(LIBMVL_CONTEXT *ctx, FILE *f)
{
ctx->f=f;
mvl_write_preamble(ctx);
}

void mvl_close(LIBMVL_CONTEXT *ctx)
{
mvl_write_directory(ctx);
mvl_write_postamble(ctx);
fflush(ctx->f);
ctx->f=NULL;
}

LIBMVL_OFFSET64 mvl_directory_length(void *data)
{
LIBMVL_VECTOR_HEADER *p=(LIBMVL_VECTOR_HEADER *)data;
if(p->type!=LIBMVL_VECTOR_OFFSET64) {
	return(0);
	}
if(p->length &1) {
	return 0;
	}
return(p->length>>1);
}

LIBMVL_OFFSET64 mvl_directory_tag(void *data, int i)
{
LIBMVL_VECTOR *p=(LIBMVL_VECTOR_HEADER *)data;
return(p->u.offset[i]);
}

LIBMVL_OFFSET64 mvl_directory_entry(void *data, int i)
{
LIBMVL_VECTOR *p=(LIBMVL_VECTOR_HEADER *)data;
return(p->u.offset[i+(p->header.length>>1)]);
}

LIBMVL_OFFSET64 mvl_find_directory_entry(LIBMVL_CONTEXT *ctx, char *tag)
{
int i;
for(i=ctx->dir_free-1;i>=0;i--) {
	if(!strcmp(tag, ctx->directory[i].tag))return(ctx->directory[i].offset);
	}
return(0);
}

void mvl_load_image(LIBMVL_CONTEXT *ctx, LIBMVL_OFFSET64 length, void *data)
{
LIBMVL_PREAMBLE *pr=(LIBMVL_PREAMBLE *)data;
LIBMVL_POSTAMBLE *pa=(LIBMVL_POSTAMBLE *)&(((unsigned char *)data)[length-sizeof(LIBMVL_POSTAMBLE)]);
LIBMVL_VECTOR *dir, *a;
int i;

if(strncmp(pr->signature, LIBMVL_SIGNATURE, 4)) {
	mvl_set_error(ctx, LIBMVL_ERR_INVALID_SIGNATURE);
	return;
	}

if(pr->endianness!=LIBMVL_ENDIANNESS_FLAG) {
	mvl_set_error(ctx, LIBMVL_ERR_WRONG_ENDIANNESS);
	return;
	}

fprintf(stderr, "Reading MVL directory at offset 0x%08lx\n", pa->directory);
dir=(LIBMVL_VECTOR *)&(((unsigned char *)data)[pa->directory]);

for(i=0;i<ctx->dir_free;i++) {
	free(ctx->directory[i].tag);
	ctx->directory[i].tag=NULL;
	ctx->directory[i].offset=0;
	}

ctx->dir_free=dir->header.length>>1;
fprintf(stderr, "Reading MVL with %ld directory entries\n", ctx->dir_free);
if(ctx->dir_free >= ctx->dir_size) {
	ctx->dir_size=ctx->dir_free;
	free(ctx->directory);
	ctx->directory=do_malloc(ctx->dir_size, sizeof(*ctx->directory));
	}
	
for(i=0;i<ctx->dir_free;i++) {
	ctx->directory[i].offset=dir->u.offset[i+ctx->dir_free];
	a=(LIBMVL_VECTOR *)&(((unsigned char *)data)[dir->u.offset[i]]);
	ctx->directory[i].tag=strndup(a->u.b, a->header.length);
	}
}
