#include <stdio.h>
#ifndef __WIN32__
#include <sys/mman.h>
#else
#include <windows.h>
#include <winbase.h>
#include <io.h>
#endif
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
#ifdef __WIN32__
	HANDLE f_handle;
	HANDLE f_map_handle;
#endif
	int modified;
	} MMAPED_LIBRARY;
	
MMAPED_LIBRARY *libraries=NULL;
int libraries_size=0;
int libraries_free=0;

SEXP mmap_library(SEXP filename, SEXP mode0)
{
int i, mode;
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
	if(libraries[i].ctx==NULL) {
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
if(idx<0) {
	idx=libraries_free;
	libraries_free++;
	}

fn=CHAR(asChar(filename));

//Rprintf("Accessing MVL library from %s\n", fn);

p=&libraries[idx];
memset(p, 0, sizeof(*p));

#ifdef __WIN32__
switch(mode) {
	case 0:
		p->f=fopen(fn, "rb");
		break;
	case 1:
		p->f=fopen(fn, "rb+");
		break;
	case 2: 
		p->f=fopen(fn, "wb");
		break;
	case 3:
		p->f=fopen(fn, "wb+");
		break;
	default:
		error("Unknown mode %d", mode);
		return(R_NilValue);
		
	}
#else 
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
#endif
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
#ifdef __WIN32__
	p->f_handle=(HANDLE)_get_osfhandle(fileno(p->f));
	if(p->f_handle==NULL) {
		error("Cannot obtain Win32 file handle");
		fclose(p->f);
		p->f=NULL;
		return(R_NilValue);
		}
	
	p->f_map_handle=CreateFileMappingA(p->f_handle, NULL, PAGE_READONLY, 0, 0, NULL);
	if(p->f_map_handle==NULL) {
		error("Cannot obtain Win32 file mapping object");
		fclose(p->f);
		p->f=NULL;
		return(R_NilValue);
		}
		
	p->data=MapViewOfFile(p->f_map_handle, FILE_MAP_READ, 0, 0, p->length);
	if(p->data==NULL) {
		error("Cannot create Win32 File mapping view");
		fclose(p->f);
		p->f=NULL;
		return(R_NilValue);
		}

#else
	p->data=mmap(NULL, p->length, PROT_READ, MAP_SHARED, fileno(p->f), 0);
	if(p->data==NULL) {
		error("Memory mapping MVL library: %s", strerror(errno));
		fclose(p->f);
		p->f=NULL;
		return(R_NilValue);
		}
#endif
	mvl_load_image(p->ctx, p->length, p->data);
	fseek(p->f, 0, SEEK_END);
	if(mode==0) {
		/* Read-only mapping; no need to use up a file descriptor */
		fclose(p->f);
		p->f=NULL;
		p->ctx->f=NULL;
		}
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

if(p->ctx==NULL)return(R_NilValue);

if(p->data!=NULL) {
#ifndef __WIN32__
	munmap(p->data, p->length);
#else
	UnmapViewOfFile(p->data);
	CloseHandle(p->f_map_handle);
#endif
	p->data=NULL;
	}
	
if(p->modified) {
	mvl_close(p->ctx);
	}
	
mvl_free_context(p->ctx);
p->ctx=NULL;
if(p->f!=NULL)fclose(p->f);
p->f=NULL;

return(R_NilValue);
}


SEXP VECTOR_ELT_STR(SEXP list, const char *s)
{
SEXP elt=R_NilValue;
SEXP names=getAttrib(list, R_NamesSymbol);

if(xlength(names)<xlength(list))return(R_NilValue);

for (long i=0; i<xlength(list); i++)
	if(!strcmp(CHAR(STRING_ELT(names, i)), s)) {
		elt = VECTOR_ELT(list, i);
		break;
		}
return elt;
}


void decode_mvl_object(SEXP obj, int *idx, LIBMVL_OFFSET64 *ofs)
{
SEXP sidx, sofs;
double doffset;
LIBMVL_OFFSET64 *offset=(LIBMVL_OFFSET64 *)&doffset;

sidx=VECTOR_ELT_STR(obj, "handle");
sofs=VECTOR_ELT_STR(obj, "offset");

*idx=-1;
*ofs=0;

if(sidx!=R_NilValue && length(sidx)==1)*idx=INTEGER(sidx)[0];

if((*idx)>=0 && sofs!=R_NilValue && length(sofs)==1) {
	doffset=REAL(sofs)[0];
	*ofs=*offset;
	}
}

LIBMVL_VECTOR * get_mvl_vector(int idx, LIBMVL_OFFSET64 offset)
{
if(idx<0 || idx>=libraries_free || offset==0)return(NULL);

if(libraries[idx].ctx==NULL)return(NULL);

if(libraries[idx].data==NULL)return(NULL);

return((LIBMVL_VECTOR *)(&libraries[idx].data[offset]));
}


int get_indices(SEXP indices, LIBMVL_VECTOR *vector, LIBMVL_OFFSET64 *N0, LIBMVL_OFFSET64 **v_idx0)
{
LIBMVL_OFFSET64 *v_idx;
LIBMVL_OFFSET64 N, count, data_offset;
int data_idx;
LIBMVL_VECTOR *vec;

double *pd;
int *pi;

*N0=0;
*v_idx0=NULL;

switch(TYPEOF(indices)) {
	case VECSXP:
		decode_mvl_object(indices, &data_idx, &data_offset);
		vec=get_mvl_vector(data_idx, data_offset);
		
		if(vec==NULL) {
			error("Invalid MVL object or R vector passed as indices");
			return(-1);
			}
		N=mvl_vector_length(vec);
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			return(-2);
			}
		
		switch(mvl_vector_type(vec)) {
			case LIBMVL_VECTOR_OFFSET64:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).i64[m]-1;
//				memcpy(v_idx, mvl_vector_data(vec).offset, N*sizeof(*v_idx));
				break;
			case LIBMVL_VECTOR_INT32:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).i[m]-1;
				break;
			case LIBMVL_VECTOR_INT64:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).i64[m]-1;
				break;
			case LIBMVL_VECTOR_DOUBLE:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).d[m]-1;
				break;
			case LIBMVL_VECTOR_FLOAT:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).f[m]-1;
				break;
			default:
				error("Cannot interpret MVL object as indices");
				return(-3);
				break;
			}
		break;
	case REALSXP:
		N=xlength(indices);
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			return(-4);
			}
		pd=REAL(indices);
		for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=pd[m]-1;
		break;
	case INTSXP:
		N=xlength(indices);
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			return(-5);
			}
		pi=INTEGER(indices);
		for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=pi[m]-1;
		break;
	case LGLSXP:
		N=xlength(indices);
		pi=LOGICAL(indices);
		count=0;
		for(LIBMVL_OFFSET64 m=0;m<N;m++)if(pi[m])count++;
		
		v_idx=calloc(count, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			return(-5);
			}
		for(LIBMVL_OFFSET64 m=0, k=0;m<N;m++)
			if(pi[m]) {
				v_idx[k]=m;
				k++;
				}
		break;
	case NILSXP:
		if(vector==NULL) {
			error("Cannot infer vector length");
			return(-6);
			}
		N=mvl_vector_length(vector);
		if(mvl_vector_type(vector)==LIBMVL_PACKED_LIST64)N--;
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			return(-7);
			}
		for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=m;
		break;
	default:
		error("Cannot interpret R object as index");
		return(-8);		
	}
*N0=N;
*v_idx0=v_idx;
return(0);
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
if(libraries[idx].ctx==NULL){
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

double *dp;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
	return(R_NilValue);
	}
idx=INTEGER(idx0)[0];
if(idx<0 || idx>=libraries_free) {
	error("no such library");
	return(R_NilValue);
	}
if(libraries[idx].ctx==NULL){
	error("no such library");
	return(R_NilValue);
	}
ans=PROTECT(allocVector(REALSXP, libraries[idx].ctx->dir_free));
names=PROTECT(allocVector(STRSXP, libraries[idx].ctx->dir_free));
dp=REAL(ans);
for(i=0;i<libraries[idx].ctx->dir_free;i++) {
	SET_STRING_ELT(names, i, mkChar(libraries[idx].ctx->directory[i].tag));
	offset=libraries[idx].ctx->directory[i].offset;
	dp[i]=*doffset;
	}
setAttrib(ans, R_NamesSymbol, names);
class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(3);
return(ans);
}

SEXP read_lengths(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans;
long i;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
LIBMVL_VECTOR *vec;
double *d_ans, *d_offsets;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(REALSXP, xlength(offsets)));
d_ans=REAL(ans);
d_offsets=REAL(offsets);
for(i=0;i<xlength(offsets);i++) {
	doffset=d_offsets[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		REAL(ans)[i]=NA_REAL;
		continue;
		}
	vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
	d_ans[i]=mvl_vector_length(vec);
	}

UNPROTECT(1);
return(ans);
}

SEXP read_types(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans;
long i;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
LIBMVL_VECTOR *vec;

int *p_ans;
double *p_offsets;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(INTSXP, xlength(offsets)));
p_ans=INTEGER(ans);
p_offsets=REAL(offsets);
for(i=0;i<xlength(offsets);i++) {
	doffset=p_offsets[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		INTEGER(ans)[i]=NA_INTEGER;
		continue;
		}
	vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
	p_ans[i]=mvl_vector_type(vec);
	}

UNPROTECT(1);
return(ans);
}

/* Prefer raw vectors for types with no exact R equivalent */
SEXP read_vectors_raw(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans, v, class;
long i, j;
long field_size;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;

double *pd;
int *pi;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
		case LIBMVL_VECTOR_CSTRING:
				field_size=1;
				break;
		case LIBMVL_VECTOR_FLOAT:
		case LIBMVL_VECTOR_INT32:
				field_size=4;
				break;
		case LIBMVL_VECTOR_DOUBLE:
		case LIBMVL_VECTOR_INT64:
		case LIBMVL_VECTOR_OFFSET64:
				field_size=8;
				break;
		default:
			field_size=1;
		}
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
		case LIBMVL_VECTOR_INT64:
		case LIBMVL_VECTOR_FLOAT:
			v=PROTECT(allocVector(RAWSXP, mvl_vector_length(vec)*field_size));
			pc=RAW(v);
			for(j=0;j<mvl_vector_length(vec)*field_size;j++)
				pc[j]=mvl_vector_data(vec).b[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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
			pi=INTEGER(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pi[j]=mvl_vector_data(vec).i[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			pd=REAL(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pd[j]=mvl_vector_data(vec).d[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			pd=REAL(v);
			for(j=0;j<mvl_vector_length(vec);j++) {
				offset=mvl_vector_data(vec).offset[j];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, mvl_vector_length(vec)-1));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<mvl_vector_length(vec)-1;j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, j), mvl_packed_list_get_entry_bytelength(vec, j)));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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


SEXP read_vectors_idx_raw(SEXP idx0, SEXP offsets, SEXP indicies)
{
int idx;
SEXP ans, v, class;
long i, j, field_size;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;

double *pd;
int *pi, *pidx;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(VECSXP, xlength(offsets)));
pidx=INTEGER(indicies);
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
		case LIBMVL_VECTOR_CSTRING:
				field_size=1;
				break;
		case LIBMVL_VECTOR_FLOAT:
		case LIBMVL_VECTOR_INT32:
				field_size=4;
				break;
		case LIBMVL_VECTOR_DOUBLE:
		case LIBMVL_VECTOR_INT64:
		case LIBMVL_VECTOR_OFFSET64:
				field_size=8;
				break;
		default:
			field_size=1;
		}
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)));
			pc=RAW(v);
			for(j=0;j<xlength(indicies);j++)
				pc[j]=mvl_vector_data(vec).b[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			error("String subset not supported");
			return(R_NilValue);
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, xlength(indicies)));
			pi=INTEGER(v);
			for(j=0;j<xlength(indicies);j++)
				pi[j]=mvl_vector_data(vec).i[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)*field_size));
			for(j=0;j<xlength(indicies)*field_size;j+=field_size)
				memcpy(&(RAW(v)[j]), &(mvl_vector_data(vec).i64[pidx[j]]), field_size);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);			
			break;
		case LIBMVL_VECTOR_FLOAT:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)*field_size));
			pc=RAW(v);
			for(j=0;j<xlength(indicies)*field_size;j+=field_size)
				memcpy(&(pc[j]), &(mvl_vector_data(vec).i64[pidx[j]]), field_size);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);			
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).d[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++) {
				offset=mvl_vector_data(vec).offset[pidx[j]];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, xlength(indicies)));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<xlength(indicies);j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, pidx[j]), mvl_packed_list_get_entry_bytelength(vec, pidx[j])));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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


/* Same as function above, but the indices are assumed to be doubles - this is a workaround against R's lack for 64-bit integers */
SEXP read_vectors_idx_raw_real(SEXP idx0, SEXP offsets, SEXP indicies)
{
int idx;
SEXP ans, v, class;
long i, j, field_size;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;

double *pd, *pidx;
int *pi;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(VECSXP, xlength(offsets)));
pidx=REAL(indicies);
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
		case LIBMVL_VECTOR_CSTRING:
				field_size=1;
				break;
		case LIBMVL_VECTOR_FLOAT:
		case LIBMVL_VECTOR_INT32:
				field_size=4;
				break;
		case LIBMVL_VECTOR_DOUBLE:
		case LIBMVL_VECTOR_INT64:
		case LIBMVL_VECTOR_OFFSET64:
				field_size=8;
				break;
		default:
			field_size=1;
		}
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)));
			pc=RAW(v);
			for(j=0;j<xlength(indicies);j++)
				pc[j]=mvl_vector_data(vec).b[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			error("String subset not supported");
			return(R_NilValue);
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, xlength(indicies)));
			pi=INTEGER(v);
			for(j=0;j<xlength(indicies);j++)
				pi[j]=mvl_vector_data(vec).i[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)*field_size));
			pc=RAW(v);
			for(j=0;j<xlength(indicies)*field_size;j+=field_size)
				memcpy(&(pc[j]), &(mvl_vector_data(vec).i64[(LIBMVL_OFFSET64)pidx[j]]), field_size);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);			
			break;
		case LIBMVL_VECTOR_FLOAT:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)*field_size));
			pc=RAW(v);
			for(j=0;j<xlength(indicies)*field_size;j+=field_size)
				memcpy(&(pc[j]), &(mvl_vector_data(vec).f[(LIBMVL_OFFSET64)pidx[j]]), field_size);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);			
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).d[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++) {
				offset=mvl_vector_data(vec).offset[(LIBMVL_OFFSET64)pidx[j]];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, xlength(indicies)));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<xlength(indicies);j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, (LIBMVL_OFFSET64)pidx[j]), mvl_packed_list_get_entry_bytelength(vec, (LIBMVL_OFFSET64)pidx[j])));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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

SEXP read_vectors_idx_raw2(SEXP idx0, SEXP offsets, SEXP indicies)
{
int idx;
SEXP ans, v, class;
long i, j, field_size;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;
LIBMVL_OFFSET64 *v_idx, N, m, k;

double *pd;
int *pi;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
	
if(get_indices(indicies, NULL, &N, &v_idx)!=0) {
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

	m=mvl_vector_length(vec);
	for(j=0;j<N;j++) {
		k=v_idx[j];
		if((k<0) || (k>m)) {
			UNPROTECT(1);
			error("Index is out of range");
			free(v_idx);
			return(R_NilValue);
			}
		}
	
	field_size=mvl_element_size(mvl_vector_type(vec));
	
	/* Unknown type */
	if(field_size<1) {
		SET_VECTOR_ELT(ans, i, R_NilValue);
		continue;
		}

	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, N));
			pc=RAW(v);
			for(j=0;j<N;j++)
				pc[j]=mvl_vector_data(vec).b[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			error("String subset not supported");
			free(v_idx);
			return(R_NilValue);
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, N));
			pi=INTEGER(v);
			for(j=0;j<N;j++)
				pi[j]=mvl_vector_data(vec).i[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			v=PROTECT(allocVector(RAWSXP, N*field_size));
			pc=RAW(v);
			for(j=0;j<N*field_size;j+=field_size)
				memcpy(&(pc[j]), &(mvl_vector_data(vec).i64[v_idx[j]]), field_size);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);			
			break;
		case LIBMVL_VECTOR_FLOAT:
			v=PROTECT(allocVector(RAWSXP, N*field_size));
			pc=RAW(v);
			for(j=0;j<N*field_size;j+=field_size)
				memcpy(&(pc[j]), &(mvl_vector_data(vec).f[v_idx[j]]), field_size);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);			
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, N));
			pd=REAL(v);
			for(j=0;j<N;j++)
				pd[j]=mvl_vector_data(vec).d[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, N));
			pd=REAL(v);
			for(j=0;j<N;j++) {
				offset=mvl_vector_data(vec).offset[v_idx[j]];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, N));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<N;j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, v_idx[j]), mvl_packed_list_get_entry_bytelength(vec, v_idx[j])));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		default:
			warning("Unknown vector type");
			SET_VECTOR_ELT(ans, i, R_NilValue);
			break;
		}
	}

UNPROTECT(1);
free(v_idx);
return(ans);
}


SEXP get_vector_data_ptr(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans;
long i;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;

double *p_offsets, *p_ans;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(REALSXP, xlength(offsets)));
p_ans=REAL(ans);
p_offsets=REAL(offsets);
for(i=0;i<xlength(offsets);i++) {
	doffset=p_offsets[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		p_ans[i]=0;
		continue;
		}
	vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
	
	offset=(LIBMVL_OFFSET64)&(mvl_vector_data(vec));
	p_ans[i]=*doffset2;	
	}
UNPROTECT(1);
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

double *pd;
int *pi;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
			pc=RAW(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pc[j]=mvl_vector_data(vec).b[j];
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
			pi=INTEGER(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pi[j]=mvl_vector_data(vec).i[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			warning("Converted 64-bit integers to doubles");
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			pd=REAL(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pd[j]=mvl_vector_data(vec).i64[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_FLOAT:
			warning("Converted 32-bit floats to doubles");
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			pd=REAL(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pd[j]=mvl_vector_data(vec).f[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			pd=REAL(v);
			for(j=0;j<mvl_vector_length(vec);j++)
				pd[j]=mvl_vector_data(vec).d[j];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, mvl_vector_length(vec)));
			pd=REAL(v);
			for(j=0;j<mvl_vector_length(vec);j++) {
				offset=mvl_vector_data(vec).offset[j];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, mvl_vector_length(vec)-1));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<mvl_vector_length(vec)-1;j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, j), mvl_packed_list_get_entry_bytelength(vec, j)));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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

SEXP read_vectors_idx(SEXP idx0, SEXP offsets, SEXP indicies)
{
int idx;
SEXP ans, v, class;
long i, j, k;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset, m;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;

double *pd;
int *pi, *pidx;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(VECSXP, xlength(offsets)));
pidx=INTEGER(indicies);
for(i=0;i<xlength(offsets);i++) {
	doffset=REAL(offsets)[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		SET_VECTOR_ELT(ans, i, R_NilValue);
		continue;
		}
	vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
	
	m=mvl_vector_length(vec);
	for(j=0;j<xlength(indicies);j++) {
		k=INTEGER(indicies)[j];
		if((k<0) || (k>m)) {
			UNPROTECT(1);
			error("Index is out of range");
			return(R_NilValue);
			}
		}	
	
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)));
			pc=RAW(v);
			for(j=0;j<xlength(indicies);j++)
				pc[j]=mvl_vector_data(vec).b[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			error("String subset not supported");
			return(R_NilValue);
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, xlength(indicies)));
			pi=INTEGER(v);
			for(j=0;j<xlength(indicies);j++)
				pi[j]=mvl_vector_data(vec).i[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			warning("Converted 64-bit integers to doubles");
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).i64[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_FLOAT:
			warning("Converted 32-bit floats to doubles");
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).f[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).d[pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++) {
				offset=mvl_vector_data(vec).offset[pidx[j]];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, xlength(indicies)));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<xlength(indicies);j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, pidx[j]), mvl_packed_list_get_entry_bytelength(vec, pidx[j])));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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


/* Same as function above, but the indices are assumed to be doubles - this is a workaround against R's lack for 64-bit integers */
SEXP read_vectors_idx_real(SEXP idx0, SEXP offsets, SEXP indicies)
{
int idx;
SEXP ans, v, class;
long i, j;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset, k, m;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;

double *pd, *pidx;
int *pi;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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
ans=PROTECT(allocVector(VECSXP, xlength(offsets)));
pidx=REAL(indicies);
for(i=0;i<xlength(offsets);i++) {
	doffset=REAL(offsets)[i];
	offset=*offset0;
	if(offset==0 || offset>libraries[idx].length-sizeof(LIBMVL_VECTOR_HEADER)) {
		SET_VECTOR_ELT(ans, i, R_NilValue);
		continue;
		}
	vec=(LIBMVL_VECTOR *)(&libraries[idx].data[offset]);
	
	m=mvl_vector_length(vec);
	for(j=0;j<xlength(indicies);j++) {
		k=(LIBMVL_OFFSET64)REAL(indicies)[j];
		if((k<0) || (k>m)) {
			UNPROTECT(1);
			error("Index is out of range");
			return(R_NilValue);
			}
		}
	
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, xlength(indicies)));
			pc=RAW(v);
			for(j=0;j<xlength(indicies);j++)
				pc[j]=mvl_vector_data(vec).b[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			error("String subset not supported");
			return(R_NilValue);
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, xlength(indicies)));
			pi=INTEGER(v);
			for(j=0;j<xlength(indicies);j++)
				pi[j]=mvl_vector_data(vec).i[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			warning("Converted 64-bit integers to doubles");
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).i64[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_FLOAT:
			warning("Converted 32-bit floats to doubles");
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).f[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++)
				pd[j]=mvl_vector_data(vec).d[(LIBMVL_OFFSET64)pidx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, xlength(indicies)));
			pd=REAL(v);
			for(j=0;j<xlength(indicies);j++) {
				offset=mvl_vector_data(vec).offset[(LIBMVL_OFFSET64)pidx[j]];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, xlength(indicies)));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<xlength(indicies);j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, (LIBMVL_OFFSET64)pidx[j]), mvl_packed_list_get_entry_bytelength(vec, (LIBMVL_OFFSET64)pidx[j])));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
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

/* This function accepts vectors in variety of formats */
SEXP read_vectors_idx2(SEXP idx0, SEXP offsets, SEXP indicies)
{
int idx;
SEXP ans, v, class;
long i, j;
double doffset;
LIBMVL_OFFSET64 *offset0=(LIBMVL_OFFSET64 *)&doffset;
LIBMVL_OFFSET64 offset, k, m;
double *doffset2=(double *)&offset;
LIBMVL_VECTOR *vec;
LIBMVL_OFFSET64 *v_idx, N;

double *pd;
int *pi;
unsigned char *pc;

if(length(idx0)!=1) {
	error("find_directory_entry first argument must be a single integer");
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

if(get_indices(indicies, NULL, &N, &v_idx)!=0) {
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
	
	m=mvl_vector_length(vec);
	for(j=0;j<N;j++) {
		k=v_idx[j];
		if((k<0) || (k>m)) {
			UNPROTECT(1);
			error("Index is out of range");
			free(v_idx);
			return(R_NilValue);
			}
		}
	
	switch(mvl_vector_type(vec)) {
		case LIBMVL_VECTOR_UINT8:
			v=PROTECT(allocVector(RAWSXP, N));
			pc=RAW(v);
			for(j=0;j<N;j++)
				pc[j]=mvl_vector_data(vec).b[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_CSTRING:
			error("String subset not supported");
			free(v_idx);
			return(R_NilValue);
			v=PROTECT(allocVector(STRSXP, 1));
			/* TODO: check that vector length is within R limits */
			SET_STRING_ELT(v, 0, mkCharLen(mvl_vector_data(vec).b, mvl_vector_length(vec)));
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			//SET_VECTOR_ELT(ans, i, mkCharLen(mvl_vector_data(v).b, mvl_vector_length(vec)));
			break;
		case LIBMVL_VECTOR_INT32:
			v=PROTECT(allocVector(INTSXP, N));
			pi=INTEGER(v);
			for(j=0;j<N;j++)
				pi[j]=mvl_vector_data(vec).i[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_INT64:
			warning("Converted 64-bit integers to doubles");
			v=PROTECT(allocVector(REALSXP, N));
			pd=REAL(v);
			for(j=0;j<N;j++)
				pd[j]=mvl_vector_data(vec).i64[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_FLOAT:
			warning("Converted 32-bit floats to doubles");
			v=PROTECT(allocVector(REALSXP, N));
			pd=REAL(v);
			for(j=0;j<N;j++)
				pd[j]=mvl_vector_data(vec).f[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			v=PROTECT(allocVector(REALSXP, N));
			pd=REAL(v);
			for(j=0;j<N;j++)
				pd[j]=mvl_vector_data(vec).d[v_idx[j]];
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			v=PROTECT(allocVector(REALSXP, N));
			pd=REAL(v);
			for(j=0;j<N;j++) {
				offset=mvl_vector_data(vec).offset[v_idx[j]];
				pd[j]=*doffset2;
				}
			class=PROTECT(allocVector(STRSXP, 1));
			SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
			classgets(v, class);
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(2);
			break;
		case LIBMVL_PACKED_LIST64:
			v=PROTECT(allocVector(STRSXP, N));
			/* TODO: check that vector length is within R limits */
			for(j=0;j<N;j++) {
				SET_STRING_ELT(v, j, mkCharLen(mvl_packed_list_get_entry(vec, libraries[idx].data, v_idx[j]), mvl_packed_list_get_entry_bytelength(vec, v_idx[j])));
				}
			SET_VECTOR_ELT(ans, i, v);
			UNPROTECT(1);
			break;
		default:
			warning("Unknown vector type");
			SET_VECTOR_ELT(ans, i, R_NilValue);
			break;
		}
	}

free(v_idx);
UNPROTECT(1);
return(ans);
}

SEXP read_metadata(SEXP idx0, SEXP offsets)
{
int idx;
SEXP ans, class;
long i;
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
if(libraries[idx].ctx==NULL) {
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
const char *ch, **strvec2;
LIBMVL_OFFSET64 *strvec;
long long *idata;
float *fdata;

double *pd;
int *pi;

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
if(libraries[idx].f==NULL) {
	error("library not open for writing");
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
	case LIBMVL_VECTOR_UINT8:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_UINT8, xlength(data), RAW(data), *moffset);
		break;
	case LIBMVL_VECTOR_INT32:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT32, xlength(data), INTEGER(data), *moffset);
		break;
	case LIBMVL_VECTOR_INT64:
		switch(TYPEOF(data)) {
			case RAWSXP:
//				Rprintf("Writing INT64 from RAW input length %lld output length %lld\n", xlength(data), xlength(data)/8);
				offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT64, xlength(data)/8, RAW(data), *moffset);
				break;
			case REALSXP:
				idata=calloc(xlength(data), sizeof(*idata));
				if(idata==NULL) {
					error("Out of memory");
					return(R_NilValue);
					}
				pd=REAL(data);
				for(i=0;i<xlength(data);i++)
					idata[i]=pd[i];
				
				offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT64, xlength(data), idata, *moffset);
				free(idata);
				break;
			case INTSXP:
				idata=calloc(xlength(data), sizeof(*idata));
				if(idata==NULL) {
					error("Out of memory");
					return(R_NilValue);
					}
				pi=INTEGER(data);
				for(i=0;i<xlength(data);i++)
					idata[i]=pi[i];
				
				offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT64, xlength(data), idata, *moffset);
				free(idata);
				break;
			default:
				error("can only write raw, double and integer to INT64");
				return(R_NilValue);
			}
		break;
	case LIBMVL_VECTOR_FLOAT:
		fdata=calloc(xlength(data), sizeof(*fdata));
		if(fdata==NULL) {
			error("Out of memory");
			return(R_NilValue);
			}
		pd=REAL(data);
		for(i=0;i<xlength(data);i++)
			fdata[i]=pd[i];
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_FLOAT, xlength(data), fdata, *moffset);
		free(fdata);
		break;
	case LIBMVL_VECTOR_DOUBLE:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_DOUBLE, xlength(data), REAL(data), *moffset);
		break;
	case LIBMVL_VECTOR_OFFSET64:
		offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_OFFSET64, xlength(data), REAL(data), *moffset);
		break;
#if 0
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
#else
	case 10000:
		strvec2=calloc(xlength(data), sizeof(*strvec2));
		if(strvec2==NULL) {
			error("Out of memory");
			return(R_NilValue);
			}
		for(i=0;i<xlength(data);i++) {
			strvec2[i]=CHAR(STRING_ELT(data, i));
			}
		offset=mvl_write_packed_list(libraries[idx].ctx, xlength(data), NULL, (char **)strvec2, *moffset);
		free(strvec2);
		break;
#endif
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

SEXP fused_write_vector(SEXP idx0, SEXP type0, SEXP data_list, SEXP metadata_offset)
{
long i, j, k, m;
int idx, data_idx, type;
double dmoffset;
LIBMVL_OFFSET64 *moffset=(LIBMVL_OFFSET64 *)&dmoffset;

LIBMVL_OFFSET64 offset, char_offset, data_offset, vec_idx, char_idx;
double *doffset=(double *)&offset;
const char *ch;
LIBMVL_OFFSET64 *strvec;
long long *idata;
float *fdata;
SEXP data;

double *pd;
int *pi;
unsigned char *pc;
long total_length, char_total_length;

LIBMVL_VECTOR *vec;

SEXP ans, class;

if(length(idx0)!=1) {
	error("fused_write_vector first argument must be a single integer");
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
if(libraries[idx].f==NULL) {
	error("library not open for writing");
	return(R_NilValue);
	}
	
if(TYPEOF(data_list)!=VECSXP) {
	error("fused_write_vector third argument must be a list of data to write out");
	return(R_NilValue);
	}

if(length(type0)!=1) {
	error("fused_write_vector second argument must be a single integer");
	return(R_NilValue);
	}
type=INTEGER(type0)[0];

libraries[idx].modified=1;

if(length(metadata_offset)<1) {
	*moffset=0;
	} else {
	dmoffset=REAL(metadata_offset)[0];
	}

total_length=0;
char_total_length=0;
for(k=0;k<xlength(data_list);k++) {
	data=VECTOR_ELT(data_list, k);
	
	if(TYPEOF(data)==STRSXP) {
		total_length+=xlength(data);
		for(i=0;i<xlength(data);i++) {
			char_total_length+=strlen(CHAR(STRING_ELT(data, i)));
			}
		continue;
		}
	
	if(TYPEOF(data)!=VECSXP) {
		total_length+=xlength(data);
		continue;
		}
	decode_mvl_object(data, &data_idx, &data_offset);
	vec=get_mvl_vector(data_idx, data_offset);
	
	if(vec==NULL) {
		error("Invalid MVL object in data list");
		return(R_NilValue);
		}
	if(mvl_vector_type(vec)!=type) {
		switch(type) {
			case 10000:
				if(mvl_vector_type(vec)!=LIBMVL_PACKED_LIST64) {
					error("Cannot convert MVL object to character");
					return(R_NilValue);
					}
				if(mvl_vector_length(vec)<1) {
					error("Invalid MVL packed list");
					return(R_NilValue);
					}
				//Rprintf("STRVEC length %ld\n", mvl_vector_length(vec));
				total_length+=mvl_vector_length(vec)-1;
				char_total_length+=mvl_vector_data(vec).offset[mvl_vector_length(vec)-1]-mvl_vector_data(vec).offset[0];
				break;
			default:
				error("Internal conversion between types of MVL objects not supported yet");
				return(R_NilValue);
			}
		} else {
		total_length+=mvl_vector_length(vec);
		}
	}

if(char_total_length>0) {
	char_offset=mvl_start_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_UINT8, char_total_length, 0, NULL, LIBMVL_NO_METADATA);
	char_offset+=sizeof(LIBMVL_VECTOR_HEADER);
	total_length++;
	offset=mvl_start_write_vector(libraries[idx].ctx, LIBMVL_PACKED_LIST64, total_length, 1, &char_offset, *moffset);
	//Rprintf("packed_list: %ld %ld %ld %ld\n", total_length, char_total_length, offset, char_offset);
	vec_idx=1;
	char_idx=0;
	} else {
	offset=mvl_start_write_vector(libraries[idx].ctx, type, total_length, 0, NULL, *moffset);
	vec_idx=0;
	char_idx=0;
	}

for(k=0;k<xlength(data_list);k++) {
	data=VECTOR_ELT(data_list, k);

	if(TYPEOF(data)==VECSXP) {
		decode_mvl_object(data, &data_idx, &data_offset);
		vec=get_mvl_vector(data_idx, data_offset);
		
		if(mvl_vector_type(vec)==type) {
			mvl_rewrite_vector(libraries[idx].ctx, type, offset, vec_idx, mvl_vector_length(vec), &(mvl_vector_data(vec)));
			vec_idx+=mvl_vector_length(vec);
			continue;
			}

		switch(type) {
			case LIBMVL_VECTOR_INT32:
				break;
			case LIBMVL_VECTOR_INT64:
				break;
			case LIBMVL_VECTOR_FLOAT:
				break;
			case LIBMVL_VECTOR_DOUBLE:
				break;
			case 10000:
				pc=(unsigned char *)get_mvl_vector(data_idx, mvl_vector_data(vec).offset[0]);
				if(pc==NULL) {
					error("Invalid packed list");
					return(R_NilValue);
					}
				mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_UINT8, char_offset-sizeof(LIBMVL_VECTOR_HEADER), char_idx, mvl_vector_data(vec).offset[mvl_vector_length(vec)-1]-mvl_vector_data(vec).offset[0], pc);
				//Rprintf("s# %ld %ld\n", vec_idx, char_idx);

				#define REWRITE_BUF_SIZE (1024*1024)
				
				strvec=calloc(REWRITE_BUF_SIZE, sizeof(*strvec));
				if(strvec==NULL) {
					error("Out of memory");
					return(R_NilValue);
					}
				
				for(j=1;j<mvl_vector_length(vec);j+=REWRITE_BUF_SIZE) {
					for(i=0;i+j<mvl_vector_length(vec) && i<REWRITE_BUF_SIZE;i++)
						strvec[i]=char_offset+char_idx+(mvl_vector_data(vec).offset[j+i]-mvl_vector_data(vec).offset[0]);
					i=j+REWRITE_BUF_SIZE>=mvl_vector_length(vec) ? mvl_vector_length(vec)-j : REWRITE_BUF_SIZE;
					mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_PACKED_LIST64, offset, vec_idx, i, strvec);
					vec_idx+=i;
					}
					
				
				/* TODO: all offsets need to shift by char_idx-mvl_vector_data(vec).offset[0] */
				
				char_idx+=mvl_vector_data(vec).offset[mvl_vector_length(vec)-1]-mvl_vector_data(vec).offset[0];
				//Rprintf("s#2 %ld %ld\n", vec_idx, char_idx);
				
				free(strvec);
				#undef REWRITE_BUF_SIZE
				break;
			default:
				break;
			}
		/* TODO */
		continue;
		}
	
	switch(type) {
		case LIBMVL_VECTOR_UINT8:
			mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_UINT8, offset, vec_idx, xlength(data), RAW(data));
			vec_idx+=xlength(data);
			break;
		case LIBMVL_VECTOR_INT32:
			mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT32, offset, vec_idx, xlength(data), INTEGER(data));
			vec_idx+=xlength(data);
			break;
		case LIBMVL_VECTOR_INT64:
			switch(TYPEOF(data)) {
				case RAWSXP:
	//				Rprintf("Writing INT64 from RAW input length %lld output length %lld\n", xlength(data), xlength(data)/8);
					mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT64, offset, vec_idx, xlength(data)/8, RAW(data));
					vec_idx+=xlength(data)/8;
					break;
				case REALSXP:
					idata=calloc(xlength(data), sizeof(*idata));
					if(idata==NULL) {
						error("Out of memory");
						return(R_NilValue);
						}
					pd=REAL(data);
					for(i=0;i<xlength(data);i++)
						idata[i]=pd[i];
					
					mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT64, offset, vec_idx, xlength(data), idata);
					vec_idx+=xlength(data);
					free(idata);
					break;
				case INTSXP:
					idata=calloc(xlength(data), sizeof(*idata));
					if(idata==NULL) {
						error("Out of memory");
						return(R_NilValue);
						}
					pi=INTEGER(data);
					for(i=0;i<xlength(data);i++)
						idata[i]=pi[i];
					
					mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_INT64, offset, vec_idx, xlength(data), idata);
					vec_idx+=xlength(data);
					free(idata);
					break;
				default:
					error("can only write raw, double and integer to INT64");
					return(R_NilValue);
				}
			break;
		case LIBMVL_VECTOR_FLOAT:
			fdata=calloc(xlength(data), sizeof(*fdata));
			if(fdata==NULL) {
				error("Out of memory");
				return(R_NilValue);
				}
			pd=REAL(data);
			for(i=0;i<xlength(data);i++)
				fdata[i]=pd[i];
			mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_FLOAT, offset, vec_idx, xlength(data), fdata);
			vec_idx+=xlength(data);
			free(fdata);
			break;
		case LIBMVL_VECTOR_DOUBLE:
			mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_DOUBLE, offset, vec_idx, xlength(data), REAL(data));
			vec_idx+=xlength(data);
			break;
		case LIBMVL_VECTOR_OFFSET64:
			mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_OFFSET64, offset, vec_idx, xlength(data), REAL(data));
			vec_idx+=xlength(data);
			break;
		case 10000:
			#define REWRITE_BUF_SIZE (1024*1024)
			
			strvec=calloc(REWRITE_BUF_SIZE, sizeof(*strvec));
			if(strvec==NULL) {
				error("Out of memory");
				return(R_NilValue);
				}
			
			/* TODO: it would be nice to bounce strings against internal buffer to reduce frequency of rewrite() calls */
			for(j=0;j<xlength(data);j+=REWRITE_BUF_SIZE) {
				for(i=0;i+j<xlength(data) && i<REWRITE_BUF_SIZE;i++) {
					ch=CHAR(STRING_ELT(data, i+j));
					m=strlen(ch);
					mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_VECTOR_UINT8, char_offset-sizeof(LIBMVL_VECTOR_HEADER), char_idx, m, ch);
					//Rprintf("str %s %d %ld\n", ch, m, char_idx);
					strvec[i]=char_offset+char_idx+m;
					char_idx+=m;
					}
				i=j+REWRITE_BUF_SIZE>=xlength(data) ? xlength(data)-j : REWRITE_BUF_SIZE;
				mvl_rewrite_vector(libraries[idx].ctx, LIBMVL_PACKED_LIST64, offset, vec_idx, i, strvec);
				vec_idx+=i;
				}
			#undef REWRITE_BUF_SIZE
			free(strvec);				
			break;
// 		case 10001:
// 			if(length(data)!=1) {
// 				error("data has to be length 1 string vector");
// 				return(R_NilValue);
// 				}
// 			ch=CHAR(STRING_ELT(data, 0));
// 			offset=mvl_write_vector(libraries[idx].ctx, LIBMVL_VECTOR_CSTRING, strlen(ch), ch, *moffset);
// 			break;
			
		default:
			error("write_vector: unknown type %d", type);
			return(R_NilValue);
		}
	}
	
if(char_idx>char_total_length) {
	error("Internal error: char_idx>char_total_length");
	}
	
ans=PROTECT(allocVector(REALSXP, 1));
REAL(ans)[0]=*doffset;

class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(2);
return(ans);
}

SEXP indexed_copy_vector(SEXP idx0, SEXP mvl_object, SEXP indices, SEXP metadata_offset)
{
LIBMVL_OFFSET64 *v_idx;
LIBMVL_OFFSET64 i, offset, data_offset, N_idx;
double *doffset=(double *)&offset;
int idx, data_idx;

int *pi;
double *pd;

double dmoffset;
LIBMVL_OFFSET64 *moffset=(LIBMVL_OFFSET64 *)&dmoffset;

LIBMVL_VECTOR *vec;

SEXP ans, class;

if(length(idx0)!=1) {
	error("fused_write_vector first argument must be a single integer");
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
if(libraries[idx].f==NULL) {
	error("library not open for writing");
	return(R_NilValue);
	}
		
if(length(metadata_offset)<1) {
	*moffset=0;
	} else {
	dmoffset=REAL(metadata_offset)[0];
	}
	
if(TYPEOF(mvl_object)!=VECSXP) {
	error("Not a valid MVL object");
	return(R_NilValue);	
	}
	
decode_mvl_object(mvl_object, &data_idx, &data_offset);
vec=get_mvl_vector(data_idx, data_offset);

if(vec==NULL) {
	error("Not a valid MVL object (2)");
	return(R_NilValue);	
	}

N_idx=xlength(indices);

v_idx=calloc(N_idx, sizeof(*v_idx));
if(v_idx==NULL) {
	error("Out of memory");
	return(R_NilValue);
	}
	
libraries[idx].modified=1;

switch(TYPEOF(indices)) {
	case REALSXP:
		pd=REAL(indices);
		for(i=0;i<N_idx;i++)v_idx[i]=pd[i]-1;
		break;
	case INTSXP:
		pi=INTEGER(indices);
		for(i=0;i<N_idx;i++)v_idx[i]=pi[i]-1;
		break;
	default:
		free(v_idx);
		error("Indices must be real or integer");
		return(R_NilValue);
	}

offset=mvl_indexed_copy_vector(libraries[idx].ctx, N_idx, v_idx, vec, libraries[data_idx].data, *moffset, 1024*1024*16);
	
free(v_idx);

ans=PROTECT(allocVector(REALSXP, 1));
REAL(ans)[0]=*doffset;

class=PROTECT(allocVector(STRSXP, 1));
SET_STRING_ELT(class, 0, mkChar("MVL_OFFSET"));
classgets(ans, class);
UNPROTECT(2);

return(ans);
}

SEXP order_vectors(SEXP data_list, SEXP indices, SEXP s_sort_function)
{
int data_idx, sort_function;
LIBMVL_OFFSET64 data_offset;
SEXP data;

double *pd;
int *pi, err;

void **vec_data;
LIBMVL_VECTOR **vectors, *vec;
LIBMVL_OFFSET64 *v_idx;
LIBMVL_OFFSET64 N;

SEXP ans;
	
if(TYPEOF(data_list)!=VECSXP) {
	error("order_vectors first argument must be a list of data to sort");
	return(R_NilValue);
	}

if(xlength(data_list)<1) {
	return(indices);
	}
	
if(TYPEOF(indices)!=NILSXP && xlength(indices)<1) {
	return(indices);
	}
	
if(TYPEOF(s_sort_function)!=INTSXP || xlength(s_sort_function)!=1) {
	error("Invalid sort function");
	return(R_NilValue);
	}

sort_function=INTEGER(s_sort_function)[0];
	
vec_data=calloc(xlength(data_list), sizeof(*vec_data));
vectors=calloc(xlength(data_list), sizeof(*vectors));
if(vec_data==NULL || vectors==NULL) {
	error("Not enough memory");
	free(vec_data);
	free(vectors);
	return(R_NilValue);
	}

for(LIBMVL_OFFSET64 k=0;k<xlength(data_list);k++) {
	data=VECTOR_ELT(data_list, k);
	decode_mvl_object(data, &data_idx, &data_offset);
	vectors[k]=get_mvl_vector(data_idx, data_offset);
	
	if(vectors[k]==NULL) {
		error("Invalid MVL object in data list");
		free(vec_data);
		free(vectors);
		return(R_NilValue);
		}
	vec_data[k]=libraries[data_idx].data;
	}
	
switch(TYPEOF(indices)) {
	case VECSXP:
		decode_mvl_object(indices, &data_idx, &data_offset);
		vec=get_mvl_vector(data_idx, data_offset);
		
		if(vec==NULL) {
			error("Invalid MVL object or R vector passed as indices");
			free(vec_data);
			free(vectors);
			return(R_NilValue);
			}
		N=mvl_vector_length(vec);
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			free(vec_data);
			free(vectors);
			return(R_NilValue);
			}
		
		switch(mvl_vector_type(vec)) {
			case LIBMVL_VECTOR_OFFSET64:
				memcpy(v_idx, mvl_vector_data(vec).offset, N*sizeof(*v_idx));
				break;
			case LIBMVL_VECTOR_INT32:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).i[m];
				break;
			case LIBMVL_VECTOR_INT64:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).i64[m];
				break;
			case LIBMVL_VECTOR_DOUBLE:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).d[m];
				break;
			case LIBMVL_VECTOR_FLOAT:
				for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=mvl_vector_data(vec).f[m];
				break;
			default:
				error("Cannot interpret MVL object as indices");
				free(vec_data);
				free(vectors);
				free(v_idx);
				return(R_NilValue);
				break;
			}
		break;
	case REALSXP:
		N=xlength(indices);
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			free(vec_data);
			free(vectors);
			return(R_NilValue);
			}
		pd=REAL(indices);
		for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=pd[m]-1;
		break;
	case INTSXP:
		N=xlength(indices);
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			free(vec_data);
			free(vectors);
			return(R_NilValue);
			}
		pi=INTEGER(indices);
		for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=pi[m]-1;
		break;
	case NILSXP:
		N=mvl_vector_length(vectors[0]);
		if(mvl_vector_type(vectors[0])==LIBMVL_PACKED_LIST64)N--;
		v_idx=calloc(N, sizeof(*v_idx));
		if(v_idx==NULL) {
			error("Not enough memory");
			free(vec_data);
			free(vectors);
			return(R_NilValue);
			}
		for(LIBMVL_OFFSET64 m=0;m<N;m++)v_idx[m]=m;
		break;
	default:
		error("Cannot interpret R object as index");
		free(vec_data);
		free(vectors);
		return(R_NilValue);		
	}
	
if((err=mvl_sort_indices(N, v_idx, xlength(data_list), vectors, vec_data, sort_function))!=0) {
	free(vec_data);
	free(vectors);
	free(v_idx);
	error("Error sorting indices, error code %d", err);
	return(R_NilValue);		
	}
ans=PROTECT(allocVector(REALSXP, N));
pd=REAL(ans);
for(LIBMVL_OFFSET64 m=0;m<N;m++)pd[m]=v_idx[m]+1;
UNPROTECT(1);
free(vec_data);
free(vectors);
free(v_idx);
return(ans);	
}

SEXP hash_vectors(SEXP data_list, SEXP indices)
{
int data_idx;
LIBMVL_OFFSET64 data_offset;
SEXP data;

double *pd;
LIBMVL_OFFSET64 *po;
int err;

void **vec_data;
LIBMVL_VECTOR **vectors;
LIBMVL_OFFSET64 *v_idx;
LIBMVL_OFFSET64 N;

SEXP ans;
	
if(TYPEOF(data_list)!=VECSXP) {
	error("order_vectors first argument must be a list of data to sort");
	return(R_NilValue);
	}

if(xlength(data_list)<1) {
	return(indices);
	}
	
if(TYPEOF(indices)!=NILSXP && xlength(indices)<1) {
	return(indices);
	}
	
vec_data=calloc(xlength(data_list), sizeof(*vec_data));
vectors=calloc(xlength(data_list), sizeof(*vectors));
if(vec_data==NULL || vectors==NULL) {
	error("Not enough memory");
	free(vec_data);
	free(vectors);
	return(R_NilValue);
	}

for(LIBMVL_OFFSET64 k=0;k<xlength(data_list);k++) {
	data=VECTOR_ELT(data_list, k);
	decode_mvl_object(data, &data_idx, &data_offset);
	vectors[k]=get_mvl_vector(data_idx, data_offset);
	
	if(vectors[k]==NULL) {
		error("Invalid MVL object in data list");
		free(vec_data);
		free(vectors);
		return(R_NilValue);
		}
	vec_data[k]=libraries[data_idx].data;
	}
	
if(get_indices(indices, vectors[0], &N, &v_idx)) {
	free(vec_data);
	free(vectors);
	return(R_NilValue);		
	}
	
ans=PROTECT(allocVector(REALSXP, N));
pd=REAL(ans);
if((err=mvl_hash_indices(N, v_idx, (LIBMVL_OFFSET64 *)pd, xlength(data_list), vectors, vec_data))!=0) {
	free(vec_data);
	free(vectors);
	free(v_idx);
	error("Error hashing indices, code %d", err);
	UNPROTECT(1);
	return(R_NilValue);		
	}
po=(LIBMVL_OFFSET64 *)pd;
for(LIBMVL_OFFSET64 m=0;m<N;m++)po[m]=(po[m] & ((1LLU<<52)-1)) | (1023LLU<<52);
UNPROTECT(1);
free(vec_data);
free(vectors);
free(v_idx);
return(ans);	
}

SEXP find_matches(SEXP data_list0, SEXP indices0, SEXP data_list1, SEXP indices1)
{
int data_idx;

LIBMVL_OFFSET64 data_offset, i, pairs_size;
SEXP data;

double *pd;
int err;

void **vec_data0, **vec_data1;
LIBMVL_VECTOR **vectors0, **vectors1;
LIBMVL_OFFSET64 *v_idx0, *v_idx1, *key_hash, *key_last, *key_match_indices, *match_indices;
LIBMVL_OFFSET64 N0, N1;

HASH_MAP *hm;

SEXP ans, obj;
	
if(TYPEOF(data_list0)!=VECSXP) {
	error("order_vectors first argument must be a list of data to merge");
	return(R_NilValue);
	}

if(TYPEOF(data_list1)!=VECSXP) {
	error("order_vectors third argument must be a list of data to merge");
	return(R_NilValue);
	}

if(xlength(data_list0)<1 || xlength(data_list1)<1) {
	error("Vector lists should not be empty");
	return(R_NilValue);
	}

if(xlength(data_list0)!=xlength(data_list1)) {
	error("Vector lists should have the same number of vectors");
	return(R_NilValue);
	}
	
if(TYPEOF(indices0)!=NILSXP && xlength(indices0)<1) {
	error("Nothing to merge");
	return(R_NilValue);
	}

if(TYPEOF(indices1)!=NILSXP && xlength(indices1)<1) {
	error("Nothing to merge");
	return(R_NilValue);
	}
	
Rprintf("Allocating vectors\n");
	
vec_data0=calloc(xlength(data_list0), sizeof(*vec_data0));
vectors0=calloc(xlength(data_list0), sizeof(*vectors0));
vec_data1=calloc(xlength(data_list1), sizeof(*vec_data1));
vectors1=calloc(xlength(data_list1), sizeof(*vectors1));
if(vec_data0==NULL || vectors0==NULL || vec_data1==NULL || vectors1==NULL) {
	error("Not enough memory");
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	return(R_NilValue);
	}
	
Rprintf("Computing data lists\n");
for(LIBMVL_OFFSET64 k=0;k<xlength(data_list0);k++) {
	data=VECTOR_ELT(data_list0, k);
	decode_mvl_object(data, &data_idx, &data_offset);
	vectors0[k]=get_mvl_vector(data_idx, data_offset);
	
	if(vectors0[k]==NULL) {
		error("Invalid MVL object in first data list");
		free(vec_data0);
		free(vectors0);
		free(vec_data1);
		free(vectors1);
		return(R_NilValue);
		}
	vec_data0[k]=libraries[data_idx].data;
	
	data=VECTOR_ELT(data_list1, k);
	decode_mvl_object(data, &data_idx, &data_offset);
	vectors1[k]=get_mvl_vector(data_idx, data_offset);
	
	if(vectors1[k]==NULL) {
		error("Invalid MVL object in second data list");
		free(vec_data0);
		free(vectors0);
		free(vec_data1);
		free(vectors1);
		return(R_NilValue);
		}
	vec_data1[k]=libraries[data_idx].data;
	
	if(mvl_vector_type(vectors0[k])!=mvl_vector_type(vectors1[k])) {
		error("Vector types do not match");
		free(vec_data0);
		free(vectors0);
		free(vec_data1);
		free(vectors1);
		return(R_NilValue);
		}
	}
	
Rprintf("Extracting index0\n");
if(get_indices(indices0, vectors0[0], &N0, &v_idx0)) {
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	return(R_NilValue);		
	}

Rprintf("Extracting index1\n");
if(get_indices(indices1, vectors1[0], &N1, &v_idx1)) {
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	free(v_idx0);
	return(R_NilValue);		
	}
	
Rprintf("Computing key hash\n");

key_hash=calloc(N0, sizeof(*key_hash));
if(key_hash==NULL) {
	error("Not enough memory");
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	free(v_idx0);
	free(v_idx1);
	return(R_NilValue);
	}

if((err=mvl_hash_indices(N0, v_idx0, key_hash, xlength(data_list0), vectors0, vec_data0))!=0) {
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	free(v_idx0);
	free(v_idx1);
	free(key_hash);
	error("Error hashing key indices %d\n", err);
	return(R_NilValue);
	}
	
// Rprintf("Allocating hash map\n");
hm=mvl_allocate_hash_map(N1);
hm->hash_count=N1;

Rprintf("Computing data hash\n");
if((err=mvl_hash_indices(N1, v_idx1, hm->hash, xlength(data_list1), vectors1, vec_data1))!=0) {
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	free(v_idx0);
	free(v_idx1);
	free(key_hash);
	mvl_free_hash_map(hm);
	error("Error hashing indices %d\n", err);
	return(R_NilValue);
	}
	
Rprintf("Computing hash map\n");
mvl_compute_hash_map(hm);

Rprintf("Estimating match count\n");
pairs_size=mvl_hash_match_count(N0, key_hash, hm);

if(pairs_size>1e9) {
	Rprintf("Expecting %lld matches\n", pairs_size);
	}
	
key_last=calloc(N0, sizeof(*key_last));
key_match_indices=calloc(pairs_size, sizeof(*key_match_indices));
match_indices=calloc(pairs_size, sizeof(*match_indices));

if(key_last==NULL || key_match_indices==NULL || match_indices==NULL) {
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	free(v_idx0);
	free(v_idx1);
	free(key_hash);
	free(key_last);
	free(key_match_indices);
	free(match_indices);
	mvl_free_hash_map(hm);
	error("Not enough memory");
	return(R_NilValue);
	}

Rprintf("Finding matches\n");
if((err=mvl_find_matches(N0, v_idx0, xlength(data_list0), vectors0, vec_data0, key_hash,
	N1, v_idx1, xlength(data_list1), vectors1, vec_data1, hm,
	key_last, pairs_size, key_match_indices, match_indices))) {
	error("Error computing merge plan %d\n", err);
	free(vec_data0);
	free(vectors0);
	free(vec_data1);
	free(vectors1);
	free(v_idx0);
	free(v_idx1);
	free(key_hash);
	free(key_last);
	free(key_match_indices);
	free(match_indices);
	mvl_free_hash_map(hm);
	return(R_NilValue);
	}

// Rprintf("Formating results\n");
	
mvl_free_hash_map(hm);
free(key_hash);
	
ans=PROTECT(allocVector(VECSXP, 3));

obj=PROTECT(allocVector(REALSXP, N0+1));
pd=REAL(obj);
pd[0]=1;

for(i=0;i<N0;i++) pd[i+1]=key_last[i]+1;

SET_VECTOR_ELT(ans, 0, obj);
UNPROTECT(1);

obj=PROTECT(allocVector(REALSXP, key_last[N0-1]));
pd=REAL(obj);

for(i=0;i<key_last[N0-1];i++) pd[i]=key_match_indices[i]+1;

SET_VECTOR_ELT(ans, 1, obj);
UNPROTECT(1);

obj=PROTECT(allocVector(REALSXP, key_last[N0-1]));
pd=REAL(obj);

for(i=0;i<key_last[N0-1];i++) pd[i]=match_indices[i]+1;

SET_VECTOR_ELT(ans, 2, obj);
UNPROTECT(1);
UNPROTECT(1);

free(vec_data0);
free(vectors0);
free(vec_data1);
free(vectors1);
free(v_idx0);
free(v_idx1);
free(key_last);
free(key_match_indices);
free(match_indices);
return(ans);
}

SEXP mvl_xlength(SEXP obj)
{
SEXP ans;

ans=PROTECT(allocVector(REALSXP, 1));
REAL(ans)[0]=xlength(obj);
UNPROTECT(1);
return(ans);
}

SEXP group_vectors(SEXP data_list, SEXP indices)
{
SEXP ans, obj, data;
int err;
double *pd, *fd;

void **vec_data;
LIBMVL_VECTOR **vectors;
LIBMVL_OFFSET64 *v_idx;
LIBMVL_OFFSET64 N;
int data_idx;

LIBMVL_OFFSET64 data_offset, i, j, k;

HASH_MAP *hm;

	
if(TYPEOF(data_list)!=VECSXP) {
	error("group_vectors first argument must be a list of data to group");
	return(R_NilValue);
	}


if(xlength(data_list)<1) {
	error("Vector lists should not be empty");
	return(R_NilValue);
	}

if(TYPEOF(indices)!=NILSXP && xlength(indices)<1) {
	error("Nothing to merge");
	return(R_NilValue);
	}

Rprintf("Allocating vectors\n");
	
vec_data=calloc(xlength(data_list), sizeof(*vec_data));
vectors=calloc(xlength(data_list), sizeof(*vectors));
if(vec_data==NULL || vectors==NULL) {
	error("Not enough memory");
	free(vec_data);
	free(vectors);
	return(R_NilValue);
	}
	
Rprintf("Computing data lists\n");
for(LIBMVL_OFFSET64 k=0;k<xlength(data_list);k++) {
	data=VECTOR_ELT(data_list, k);
	decode_mvl_object(data, &data_idx, &data_offset);
	vectors[k]=get_mvl_vector(data_idx, data_offset);
	
	if(vectors[k]==NULL) {
		error("Invalid MVL object in first data list");
		free(vec_data);
		free(vectors);
		return(R_NilValue);
		}
	vec_data[k]=libraries[data_idx].data;
	}
	
Rprintf("Extracting index\n");
if(get_indices(indices, vectors[0], &N, &v_idx)) {
	free(vec_data);
	free(vectors);
	return(R_NilValue);		
	}
	
hm=mvl_allocate_hash_map(N);
hm->hash_count=N;

Rprintf("Computing data hash\n");
if((err=mvl_hash_indices(N, v_idx, hm->hash, xlength(data_list), vectors, vec_data))!=0) {
	free(vec_data);
	free(vectors);
	free(v_idx);
	mvl_free_hash_map(hm);
	error("Error hashing indices %d\n", err);
	return(R_NilValue);
	}
	
Rprintf("Computing hash map\n");
mvl_compute_hash_map(hm);

mvl_find_groups(N, v_idx, xlength(data_list), vectors, vec_data, hm);

ans=PROTECT(allocVector(VECSXP, 2));

obj=PROTECT(allocVector(REALSXP, N));
data=PROTECT(allocVector(REALSXP, hm->first_count+1));

pd=REAL(obj);
fd=REAL(data);
fd[0]=1;

j=0;
for(i=0;i<hm->first_count;i++) {
	k=hm->first[i];
	while(k!=~0LLU) {
		pd[j]=k+1;
		j++;
		k=hm->next[k];
		}
	fd[i+1]=j+1;
	}
	
SET_VECTOR_ELT(ans, 0, data);
SET_VECTOR_ELT(ans, 1, obj);

free(vec_data);
free(vectors);
free(v_idx);
mvl_free_hash_map(hm);

UNPROTECT(3);
return(ans);
}


void R_init_RMVL(DllInfo *info) {
  R_RegisterCCallable("RMVL", "mmap_library",  (DL_FUNC) &mmap_library);
  R_RegisterCCallable("RMVL", "close_library",  (DL_FUNC) &close_library);
  R_RegisterCCallable("RMVL", "find_directory_entries",  (DL_FUNC) &find_directory_entries);
  R_RegisterCCallable("RMVL", "get_directory",  (DL_FUNC) &get_directory);
  R_RegisterCCallable("RMVL", "read_metadata",  (DL_FUNC) &read_metadata);
  R_RegisterCCallable("RMVL", "read_lengths",  (DL_FUNC) &read_lengths);
  R_RegisterCCallable("RMVL", "read_types",  (DL_FUNC) &read_types);
  R_RegisterCCallable("RMVL", "get_vector_data_ptr",  (DL_FUNC) &get_vector_data_ptr);
  R_RegisterCCallable("RMVL", "read_vectors_raw",  (DL_FUNC) &read_vectors_raw);
  R_RegisterCCallable("RMVL", "read_vectors_idx_raw",  (DL_FUNC) &read_vectors_idx_raw);
  R_RegisterCCallable("RMVL", "read_vectors_idx_raw_real",  (DL_FUNC) &read_vectors_idx_raw_real);
  R_RegisterCCallable("RMVL", "read_vectors_idx_raw2",  (DL_FUNC) &read_vectors_idx_raw2);
  R_RegisterCCallable("RMVL", "read_vectors",  (DL_FUNC) &read_vectors);
  R_RegisterCCallable("RMVL", "read_vectors_idx",  (DL_FUNC) &read_vectors_idx);
  R_RegisterCCallable("RMVL", "read_vectors_idx_real",  (DL_FUNC) &read_vectors_idx_real);
  R_RegisterCCallable("RMVL", "read_vectors_idx2",  (DL_FUNC) &read_vectors_idx2);
  R_RegisterCCallable("RMVL", "add_directory_entries",  (DL_FUNC) &add_directory_entries);
  R_RegisterCCallable("RMVL", "write_vector",  (DL_FUNC) &write_vector);
  R_RegisterCCallable("RMVL", "fused_write_vector",  (DL_FUNC) &fused_write_vector);
  R_RegisterCCallable("RMVL", "order_vectors",  (DL_FUNC) &order_vectors);
  R_RegisterCCallable("RMVL", "hash_vectors",  (DL_FUNC) &hash_vectors);
  R_RegisterCCallable("RMVL", "find_matches",  (DL_FUNC) &find_matches);
  R_RegisterCCallable("RMVL", "indexed_copy_vector",  (DL_FUNC) &indexed_copy_vector);
  R_RegisterCCallable("RMVL", "mvl_xlength",  (DL_FUNC) &mvl_xlength);
  R_RegisterCCallable("RMVL", "group_vectors",  (DL_FUNC) &group_vectors);
}
