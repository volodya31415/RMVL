#include <stdio.h>
#include <sys/mman.h>
#include <errno.h>
#include "libMVL.h"
#include <R.h>
#include <Rinternals.h>
//#include <Rext/Print.h>

typedef struct {
	FILE *f;
	char *data;
	LIBMVL_OFFSET64 length;
	LIBMVL_CONTEXT *ctx;
	int modified;
	} MMAPED_LIBRARY;
	
MMAPED_LIBRARY *libraries=NULL;
int libraries_size=0;
int libraries_free=0;

SEXP mmap_library(SEXP filename, SEXP mode0)
{
LIBMVL_OFFSET64 length, offset;
int i, count, mode;
int idx;
MMAPED_LIBRARY *p;
const char *fn;
SEXP ans;

if(length(mode0)!=1) {
	error("mmap_library argument mode has to be length 1 integer");
	return(R_NilValue);
	}
	
mode=INTEGER(mode0)[0];

idx=-1;
for(i=0;i<libraries_free;i++) {
	if(libraries[i].f==NULL) {
		idx=i;
		break;
		}
	}

if(idx<0 && (libraries_free>=libraries_size)) {
	libraries_size=2*libraries_size+10;
	p=calloc(libraries_size, sizeof(*libraries));
	if(p==NULL) {
		error("Opening MVL library \"%s\": out of memory", fn);
		return(R_NilValue);
		}
	if(libraries_free>0)memcpy(p, libraries, libraries_free*sizeof(*libraries));
	free(libraries);
	libraries=p;
	}
idx=libraries_free;
libraries_free++;

fn=CHAR(asChar(filename));

Rprintf("Accessing MVL library from %s\n", fn);

p=&libraries[idx];
memset(p, 0, sizeof(*p));

switch(mode) {
	case 0:
		p->f=fopen(fn, "r");
		break;
	case 1:
		p->f=fopen(fn, "r+");
		break;
	case 2: 
		p->f=fopen(fn, "w");
		break;
	case 3:
		p->f=fopen(fn, "w+");
		break;
	default:
		error("Unknown mode %d", mode);
		return(R_NilValue);
		
	}
if(p->f==NULL) {
	error("Opening MVL library \"%s\": %s", fn, strerror(errno));
	return(R_NilValue);
	}
	
fseek(p->f, 0, SEEK_END);
p->length=ftell(p->f);
fseek(p->f, 0, SEEK_SET);

p->ctx=mvl_create_context();
p->ctx->f=p->f;

if(p->length>0) {
	p->data=mmap(NULL, libraries[idx].length, PROT_READ, MAP_SHARED, fileno(p->f), 0);
	if(p->data==NULL) {
		error("Memory mapping MVL library: %s", strerror(errno));
		fclose(p->f);
		p->f=NULL;
		return(R_NilValue);
		}
	mvl_load_image(p->ctx, p->length, p->data);
	fseek(p->f, 0, SEEK_END);
	} else {
	mvl_write_preamble(p->ctx);
	p->modified=1;
	}
	
ans=PROTECT(allocVector(INTSXP, 1));
INTEGER(ans)[0]=idx;
UNPROTECT(1);
return(ans);
}

SEXP close_library(SEXP idx0)
{
int idx; 
MMAPED_LIBRARY *p;
if(length(idx0)!=1) {
	error("close_library requires a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0)return(R_NilValue);
if(idx>=libraries_free)return(R_NilValue);

p=&(libraries[idx]);

if(p->f==NULL)return(R_NilValue);

if(p->data!=NULL) {
	munmap(p->data, p->length);
	p->data=NULL;
	}
	
if(p->modified) {
	mvl_close(p->ctx);
	}
	
mvl_free_context(p->ctx);
p->ctx=NULL;
fclose(p->f);
p->f=NULL;

return(R_NilValue);
}

SEXP find_directory_entries(SEXP idx0, SEXP tag)
{
int idx;
SEXP ans, class;
const char *tag0;
long i;
LIBMVL_OFFSET64 offset;
double *doffset=(double *)&offset;
if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].f==NULL){
	error("no such library");
	return(R_NilValue);
	}
ans=PROTECT(allocVector(REALSXP, xlength(tag)));
for(i=0;i<xlength(tag);i++) {
	tag0=CHAR(STRING_ELT(tag, i));
	offset=mvl_find_directory_entry(libraries[idx].ctx, tag0);
	REAL(ans)[i]=*doffset;
	}

class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(2);
return(ans);
}

SEXP get_directory(SEXP idx0)
{
int idx;
SEXP ans, class, names;
long i;
LIBMVL_OFFSET64 offset;
double *doffset=(double *)&offset;
if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].f==NULL){
	error("no such library");
	return(R_NilValue);
	}
ans=PROTECT(allocVector(REALSXP, libraries[idx].ctx->dir_free));
names=PROTECT(allocVector(STRSXP, libraries[idx].ctx->dir_free));
for(i=0;i<libraries[idx].ctx->dir_free;i++) {
	SET_STRING_ELT(names, i, mkChar(libraries[idx].ctx->directory[i].tag));
	offset=libraries[idx].ctx->directory[i].offset;
	REAL(ans)[i]=*doffset;
	}
setAttrib(ans, R_NamesSymbol, names);
class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(3);
return(ans);
}

SEXP read_vectors(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans, v, class;
long i, j;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;
if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].f==NULL) {
	error("no such library");
	return(R_NilValue);
	}
ans=PROTECT(allocVector(VECSXP, xlength(offsets)));
for(i=0;i<xlength(offsets);i++) {
	doffset=REAL(offsets)[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		SET_VECTOR_ELT(ans, i, R_NilValue);
		continue;
		}
	vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, mvl_vector_length(vec)));
			for(j=0;j<mvl_vector_length(vec);j++)
				RAW(v)[j]=mvl_vector_data(vec).b[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, mvl_vector_length(vec)));
			for(j=0;j<mvl_vector_length(vec);j++)
				INTEGER(v)[j]=mvl_vector_data(vec).i[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			warning("Converted 64-bit integers to doubles");
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			for(j=0;j<mvl_vector_length(vec);j++)
				REAL(v)[j]=mvl_vector_data(vec).i64[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_FLOAT:
			warning("Converted 32-bit floats to doubles");
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			for(j=0;j<mvl_vector_length(vec);j++)
				REAL(v)[j]=mvl_vector_data(vec).f[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			for(j=0;j<mvl_vector_length(vec);j++)
				REAL(v)[j]=mvl_vector_data(vec).d[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			for(j=0;j<mvl_vector_length(vec);j++) {
				offset=mvl_vector_data(vec).offset[j];
				REAL(v)[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		default:
			warning("Unknown vector type");
			SET_VECTOR_ELT(ans, i, R_NilValue);
			break;
		}
	}

UNPROTECT(1);
return(ans);
}

SEXP read_metadata(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans, v, class;
long i, j;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;
if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].f==NULL) {
	error("no such library");
	return(R_NilValue);
	}
ans=PROTECT(allocVector(REALSXP, xlength(offsets)));
for(i=0;i<xlength(offsets);i++) {
	doffset=REAL(offsets)[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		offset=0;
		} else {
		vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
		offset=mvl_vector_metadata_offset(vec);
		}
	REAL(ans)[i]=*doffset2;
	}

class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(2);
return(ans);
}

SEXP add_directory_entries(SEXP idx0, SEXP tags, SEXP offsets)
{
long i;
int idx;
double doffset;
LIBMVL_OFFSET64 *offset=(LIBMVL_OFFSET64 *)&doffset;
if(length(idx0)!=1) {
	error("add_directory_entries first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].ctx==NULL) {
	error("no such library");
	return(R_NilValue);
	}
if(xlength(tags)!=xlength(offsets)) {
	error("add_directory_entries requires number of tags to match number of offsets");
	return(R_NilValue);
	}
for(i=0;i<xlength(tags);i++) {
	doffset=REAL(offsets)[i];
	mvl_add_directory_entry(libraries[idx].ctx, *offset, CHAR(STRING_ELT(tags, i)));
	}
return(R_NilValue);
}

SEXP write_vector(SEXP idx0, SEXP type0, SEXP data, SEXP metadata_offset)
{
long i;
int idx, type;
double dmoffset;
LIBMVL_OFFSET64 *moffset=(LIBMVL_OFFSET64 *)&dmoffset;

LIBMVL_OFFSET64 offset;
double *doffset=(double *)&offset;
const char *ch;
LIBMVL_OFFSET64 *strvec;

SEXP ans, class;

if(length(idx0)!=1) {
	error("write_vector first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].ctx==NULL) {
	error("no such library");
	return(R_NilValue);
	}

if(length(type0)!=1) {
	error("write_vector second argument must be a single integer");
	return(R_NilValue);
	}
type=INTEGER(type0)[0];

libraries[idx].modified=1;

if(length(metadata_offset)<1) {
	*moffset=0;
	} else {
	dmoffset=REAL(metadata_offset)[0];
	}
switch(type) {
	case LIBMVL_VECTOR_INT32:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT32, xlength(data), INTEGER(data), *moffset);
		break;
	case LIBMVL_VECTOR_DOUBLE:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_DOUBLE, xlength(data), REAL(data), *moffset);
		break;
	case LIBMVL_VECTOR_OFFSET64:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_OFFSET64, xlength(data), REAL(data), *moffset);
		break;
	case 10000:
		strvec=calloc(xlength(data), sizeof(*strvec));
		if(strvec==NULL) {
			error("Out of memory");
			return(R_NilValue);
			}
		for(i=0;i<xlength(data);i++) {
			ch=CHAR(STRING_ELT(data, i));
			strvec[i]=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_CSTRING, strlen(ch), ch, 0);
			}
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_OFFSET64, xlength(data), strvec, *moffset);
		free(strvec);
		break;
	case 10001:
		if(length(data)!=1) {
			error("data has to be length 1 string vector");
			return(R_NilValue);
			}
		ch=CHAR(STRING_ELT(data, 0));
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_CSTRING, strlen(ch), ch, *moffset);
		break;
		
	default:
		error("write_vector: unknown type %d", type);
		return(R_NilValue);
	}
ans=PROTECT(allocVector(REALSXP, 1));
REAL(ans)[0]=*doffset;

class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(2);
return(ans);
}

void R_init_RMVL(DllInfo *info) {
  R_RegisterCCallable("RMVL", "mmap_library",  (DL_FUNC) &mmap_library);
  R_RegisterCCallable("RMVL", "close_library",  (DL_FUNC) &close_library);
  R_RegisterCCallable("RMVL", "find_directory_entries",  (DL_FUNC) &find_directory_entries);
  R_RegisterCCallable("RMVL", "get_directory",  (DL_FUNC) &get_directory);
  R_RegisterCCallable("RMVL", "read_metadata",  (DL_FUNC) &read_metadata);
  R_RegisterCCallable("RMVL", "read_vectors",  (DL_FUNC) &read_vectors);
  R_RegisterCCallable("RMVL", "add_directory_entries",  (DL_FUNC) &add_directory_entries);
  R_RegisterCCallable("RMVL", "write_vector",  (DL_FUNC) &write_vector);
}
