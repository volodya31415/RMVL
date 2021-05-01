
MVL_SMALL_LENGTH<-1024

mvl_open<-function(filename, append=FALSE, create=FALSE) {
	MVLHANDLE<-list(handle=.Call("mmap_library", as.character(filename), as.integer(ifelse(append, 1, 0)+ifelse(create, 2, 0))))
	class(MVLHANDLE)<-"MVL"
	MVLHANDLE[["directory"]]<-mvl_get_directory(MVLHANDLE)
	return(MVLHANDLE)
	}
	
mvl_close<-function(MVLHANDLE) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	.Call("close_library", MVLHANDLE[["handle"]])

	return(invisible(NULL))
	}
	
mvl_get_directory<-function(MVLHANDLE) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	return(.Call("get_directory", MVLHANDLE[["handle"]]))
	}

mvl_get_vectors<-function(MVLHANDLE, offsets, raw=FALSE) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!inherits(offsets, "MVL_OFFSET"))stop("not an MVL offset")
	if(raw)
		return(.Call("read_vectors_raw", MVLHANDLE[["handle"]], offsets))
		else
		return(.Call("read_vectors", MVLHANDLE[["handle"]], offsets))
	}
	
mvl_write_vector<-function(MVLHANDLE, x, metadata.offset=NULL) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!is.null(metadata.offset) && !inherits(metadata.offset, "MVL_OFFSET"))stop("not an MVL offset")
	if(class(x)=="factor")x<-as.character(x)
	type<-attr(x, "MVL_TYPE", exact=TRUE)
	if(is.null(type)) {
		type<-switch(class(x), raw=1, numeric=5, integer=2, MVL_OFFSET=100, character=10000, -1)
		if(type<0 && class(x) %in% c("array", "matrix"))type<-switch(typeof(x), double=5, integer=2, -1)
		}
	if(type>0) {
		return(.Call("write_vector", MVLHANDLE[["handle"]], as.integer(type), x, metadata.offset)) 
		}
	stop("Could not write vector with class ", class(x))
	}

mvl_write_string<-function(MVLHANDLE, x, metadata.offset=NULL) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!is.null(metadata.offset) && !inherits(metadata.offset, "MVL_OFFSET"))stop("not an MVL offset")
	x<-as.character(x)
	if(length(x)!=1)stop("requires a single string as argument")
	return(.Call("write_vector", MVLHANDLE[["handle"]], as.integer(10001), x, metadata.offset)) 
	}
	
mvl_write_object_metadata<-function(MVLHANDLE, x) {
	n<-mvl_write_string(MVLHANDLE, "MVL_LAYOUT")
	o<-mvl_write_string(MVLHANDLE, "R")
	if(!is.null(dim(x))) {
		n<-c(n, mvl_write_string(MVLHANDLE, "dim"))
		o<-c(o, mvl_write_vector(MVLHANDLE, dim(x)))
		}
	if(!is.null(class(x)) && !(class(x) %in% c("raw", "numeric", "integer"))) {
		n<-c(n, mvl_write_string(MVLHANDLE, "class"))
		o<-c(o, mvl_write_string(MVLHANDLE, class(x)))
		}
	if(!is.null(names(x))) {
		n<-c(n, mvl_write_string(MVLHANDLE, "names"))
		o<-c(o, mvl_write_vector(MVLHANDLE, names(x)))
		}
	if(!is.null(rownames(x))) {
		n<-c(n, mvl_write_string(MVLHANDLE, "rownames"))
		o<-c(o, mvl_write_vector(MVLHANDLE, rownames(x)))
		}
	if(is.null(n))return(NULL)
	ofs<-c(n, o)
	class(ofs)<-"MVL_OFFSET"
	return(mvl_write_vector(MVLHANDLE, ofs))
	}

mvl_write_object<-function(MVLHANDLE, x, name=NULL) {
	#cat("Writing", class(x), typeof(x), "\n")
	metadata<-mvl_write_object_metadata(MVLHANDLE, x)
	if(class(x) %in% c("numeric", "character", "integer", "factor", "raw", "array", "matrix")) {
		offset<-mvl_write_vector(MVLHANDLE, x, metadata)
		if(!is.null(name))mvl_add_directory_entries(MVLHANDLE, name, offset)
		return(offset)
		}
	if(class(x) %in% c("list", "data.frame")) {
		v<-unlist(lapply(x, function(x){return(mvl_write_object(MVLHANDLE, x))}))
		class(v)<-"MVL_OFFSET"
		offset<-mvl_write_vector(MVLHANDLE, v, metadata)
		if(!is.null(name))mvl_add_directory_entries(MVLHANDLE, name, offset)
		return(offset)
		}
	if(class(x) == "MVL_OFFSET") {
		# Already written
		return(x)
		}
	stop("Could not write object with class ", class(x))
	}
	
flatten_string<-function(v) {
	return(unlist(lapply(v, function(x){return(x[[1]])})))
	}

mvl_read_metadata<-function(MVLHANDLE, metadata_offset) {
	metadata<-mvl_read_object(MVLHANDLE, metadata_offset)
	if(!is.null(metadata)) {
		n<-metadata[1:(length(metadata)/2)]
		metadata<-metadata[(length(metadata)/2+1):length(metadata)]
		names(metadata)<-unlist(n)
		}
	return(metadata)
	}
	
mvl_read_object<-function(MVLHANDLE, offset, idx=NULL, recurse=TRUE, raw=FALSE) {
	if(!inherits(MVLHANDLE, "MVL") && !inherits(MVLHANDLE, "MVL_OBJECT")) stop("not an MVL object")
	if(!inherits(offset, "MVL_OFFSET"))stop("not an MVL offset")
	if(offset==0)return(NULL)
	metadata_offset<-.Call("read_metadata", MVLHANDLE[["handle"]], offset)
	metadata<-mvl_read_metadata(MVLHANDLE, metadata_offset)
	if(is.null(idx)) {
		if(raw) 
			vec<-.Call("read_vectors_raw", MVLHANDLE[["handle"]], offset)[[1]]
			else
			vec<-.Call("read_vectors", MVLHANDLE[["handle"]], offset)[[1]]
		} else {
		if(raw)
			vec<-.Call("read_vectors_idx_raw_real", MVLHANDLE[["handle"]], offset, idx[[1]])[[1]]
			else
			vec<-.Call("read_vectors_idx_real", MVLHANDLE[["handle"]], offset, idx[[1]])[[1]]
		}
	if(inherits(vec, "MVL_OFFSET")) {
		lengths<-.Call("read_lengths", MVLHANDLE[["handle"]], vec)
		if(recurse) {
			vec<-lapply(vec, function(x){class(x)<-"MVL_OFFSET" ; return(mvl_read_object(MVLHANDLE, x, raw=raw))})
		 } else {
			Fsmall<-lengths<MVL_SMALL_LENGTH
			vec[Fsmall]<-lapply(vec[Fsmall], function(x){class(x)<-"MVL_OFFSET" ; return(mvl_read_object(MVLHANDLE, x, recurse=FALSE, raw=raw))})
			vec[!Fsmall]<-lapply(vec[!Fsmall], function(x) { 
				class(x)<-"MVL_OFFSET"
				L<-list(handle=MVLHANDLE[["handle"]], offset=x, length=.Call("read_lengths", MVLHANDLE[["handle"]], x), type=.Call("read_types", MVLHANDLE[["handle"]], x),metadata_offset=.Call("read_metadata", MVLHANDLE[["handle"]], x))
				L[["metadata"]]<-mvl_read_metadata(MVLHANDLE, L[["metadata_offset"]])
				class(L)<-"MVL_OBJECT"
				return(L)} 
				)
			}
		}
#	attr(vec, "metadata")<-metadata
	if(any(metadata[["MVL_LAYOUT"]]=="R")) {
		cl<-metadata[["class"]]
		if(!is.null(cl)) {
			if(cl!="data.frame" && !is.null(metadata[["dim"]]))dim(vec)<-metadata[["dim"]]
			if(cl=="factor" || cl=="character") {
				vec<-flatten_string(vec)
				if(cl=="factor")vec<-as.factor(vec)
				}
			class(vec)<-cl
			}
		if(!is.null(metadata[["names"]]))names(vec)<-flatten_string(metadata[["names"]])
		if(!is.null(metadata[["rownames"]]))rownames(vec)<-flatten_string(metadata[["rownames"]])
		}
	return(vec)
	}
	
mvl_add_directory_entries<-function(MVLHANDLE, tag, offsets) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!inherits(offsets, "MVL_OFFSET"))stop("not an MVL offset")
	return(.Call("add_directory_entries", MVLHANDLE[["handle"]], as.character(tag), offsets))
	}
	
`[.MVL_OFFSET`<-function(x, y) {
	z<-unclass(x)[y]
	class(z)<-"MVL_OFFSET"
	return(z)
	}

`[[.MVL_OFFSET`<-function(x, y) {
	z<-unclass(x)[[y]]
	class(z)<-"MVL_OFFSET"
	return(z)
	}
	
`[.MVL`<-function(MVLHANDLE, y, sql=FALSE, raw=FALSE) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	
	if(is.factor(y))y<-as.character(y)
	
	if(is.character(y)) {
		if(sql) {
			cat("Running sql line \"", y, "\"\n", sep="")
			return(.Call("execute_sql", MVLHANDLE[["handle"]], y))
			} else {
			offset<-MVLHANDLE[["directory"]][[y]]
			if(is.null(offset))return(NULL)
			
			obj<-list(handle=MVLHANDLE[["handle"]], offset=offset, 
				length=.Call("read_lengths", MVLHANDLE[["handle"]], offset), 
				type=.Call("read_types", MVLHANDLE[["handle"]], offset),
				metadata_offset=.Call("read_metadata", MVLHANDLE[["handle"]], offset))
			obj[["metadata"]]<-mvl_read_metadata(MVLHANDLE, obj[["metadata_offset"]])
			class(obj)<-"MVL_OBJECT"
			
			if(obj[["length"]]<MVL_SMALL_LENGTH)obj<-mvl_read_object(MVLHANDLE, obj[["offset"]], recurse=FALSE, raw=raw)
			
			return(obj)
			}
		}
	if(class(y)=="formula") {
		print(y)
		print(terms(y, keep.order=TRUE, simplify=FALSE))
		}
	stop("Cannot process ", y, " class=", class(y))
	}
	
names.MVL<-function(MVLHANDLE) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	return(names(MVLHANDLE[["directory"]]))
	}
	
print.MVL<-function(MVLHANDLE) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	cat("MVL(handle ", MVLHANDLE[["handle"]], " directory with ", length(MVLHANDLE[["directory"]]), " entries)\n", sep="")
	invisible(MVLHANDLE)
	}
	
MVL_TYPE_NAME<-list("UINT8", "INT32", "INT64", "FLOAT", "DOUBLE")
MVL_TYPE_NAME[[100]]<-"OFFSET64"
MVL_TYPE_NAME[[101]]<-"CSTRING"
	
mvl_type_name<-function(x) {
	y<-lapply(MVL_TYPE_NAME[x], function(xx){if(is.null(xx))return(NA); return(xx)})
	return(unlist(y))
	}
	
print.MVL_OBJECT<-function(obj) {
	object_class<-obj[["metadata"]][["class"]]
	if(is.null(object_class) || (object_class %in% c("numeric", "integer"))) {
		cat("MVL_OBJECT(", mvl_type_name(obj[["type"]]), " length ", obj[["length"]], ")\n", sep="")
		} else
	if(object_class %in% c("data.frame", "array", "matrix")) {
		od<-obj[["metadata"]][["dim"]]
		if(is.null(od))od<-obj[["length"]]
		cat("MVL_OBJECT(", mvl_type_name(obj[["type"]]), " ", object_class, " ", paste0(od, collapse="x"), ")\n", sep="")
		} else {
		cat("MVL_OBJECT(", mvl_type_name(obj[["type"]]), " ", object_class, ")\n", sep="")
		}
	invisible(obj)
	}
	
`[.MVL_OBJECT`<-function(obj, i, ..., drop=NULL, raw=FALSE) {
	if(missing(i) && ...length()==0) {
		return(mvl_read_object(obj, unclass(obj)[["offset"]], raw=raw))
		}
	#cat("obj class ", obj[["metadata"]][["class"]], "\n")
	object_class<-obj[["metadata"]][["class"]]
	if(is.null(object_class))object_class<-"NULL"
	if(object_class=="data.frame") {
		if(...length()>1)stop("Object", obj, "has only two dimensions")
		n<-obj[["metadata"]][["names"]]
		if(...length()<1) {
			j<-1:length(n)
			} else {
			j<-..1
			if(is.logical(j)) {
				j<-(1:length(n))[j]
				} else
			if(is.character(j) || is.factor(j)) {
				if(is.factor(j))j<-as.character(j)
				j0<-match(j, n)
				if(any(is.na(j0)))
					stop("Unknown columns ", j[is.na(j0)])
				j<-j0
				}
			n<-n[j]
			}
		if(raw)
			ofs<-.Call("read_vectors_raw", obj[["handle"]], obj[["offset"]])[[1]][j]
			else
			ofs<-.Call("read_vectors", obj[["handle"]], obj[["offset"]])[[1]][j]
#		vec<-.Call("read_vectors", obj[["handle"]], ofs)
		df<-lapply(ofs, function(x){class(x)<-"MVL_OFFSET" ; return(mvl_read_object(obj, x, idx=list(i)))})
		names(df)<-n
		class(df)<-"data.frame"
		rownames(df)<-obj[["metadata"]][["rownames"]][i]
		return(df)
		}
	if(object_class %in% c("array", "matrix")) {
		od<-obj[["metadata"]][["dim"]]
		if(is.null(od))od<-obj[["length"]]
		
		if(missing(i)) {
			d<-od[1]
			idx<-0:(od[1]-1)
			} else {
			d<-length(i)
			idx<-i-1
			}
		mult<-1
		
		if(...length()+1!=length(od))stop("Array dimension is ", length(od), " but ", ...length()+1, " indices given")
		
		if(...length()>0) {
			for(j in 1:...length()) {
				ii<-NULL
				try({ii<-...elt(j)}, silent=TRUE)
				if(is.null(ii)) {
					d<-c(d, od[j+1])
					ii<-1:od[j+1]
					} else {
					d<-c(d, length(ii))
					}
				mult<-mult*od[j]
				idx<-outer(idx, (ii-1)*mult, FUN="+")
				}
			}
		if(raw)
			vec<-.Call("read_vectors_idx_raw_real", obj[["handle"]], obj[["offset"]], as.numeric(idx))[[1]]
			else
			vec<-.Call("read_vectors_idx_real", obj[["handle"]], obj[["offset"]], as.numeric(idx))[[1]]
		
		if(is.null(drop) || drop==TRUE) {
			d<-d[d!=1]
			if(length(d)>0)dim(vec)<-d
			} else
			dim(vec)<-d
		return(vec)
		}
	if(...length()==0) {
		if(is.logical(i)) {
			i<-(1:length(i))[i]
			}
		if(is.factor(i))i<-as.character(i)
		if(is.character(i)) {
			if(is.null(obj$metadata$names))stop("Object has no names")
			i<-which.max(obj$metadata$names==i)
			}
		if(is.numeric(i)) {
			#print(i)
			#print(L)
#			vec<-mvl_read_object(obj, obj[["offset"]], idx=list(as.integer(i)), recurse=FALSE)
			if(raw)
				vec<-.Call("read_vectors_idx_raw", obj[["handle"]], obj[["offset"]], as.integer(i-1))[[1]]
				else
				vec<-.Call("read_vectors_idx", obj[["handle"]], obj[["offset"]], as.integer(i-1))[[1]]
#			vec<-.Call("read_vectors", obj[["handle"]], obj[["offset"]])[[1]][i]

			if(inherits(vec, "MVL_OFFSET") && length(vec)==1) {
				vec<-mvl_read_object(obj, vec, recurse=FALSE)
				} else {
				#metadata_offset<-.Call("read_metadata", obj[["handle"]], obj[["offset"]])
				#metadata<-mvl_read_metadata(obj, metadata_offset)
				#print(metadata)
				if(0 && any(metadata[["MVL_LAYOUT"]]=="R")) {
					cl<-metadata[["class"]]
					if(cl!="data.frame" && !is.null(metadata[["dim"]]))dim(vec)<-metadata[["dim"]]
					if(cl=="factor" || cl=="character") {
						vec<-flatten_string(vec)
						if(cl=="factor")vec<-as.factor(vec)
						}
						class(vec)<-cl
					if(!is.null(metadata[["names"]]))names(vec)<-flatten_string(metadata[["names"]])
					if(!is.null(metadata[["rownames"]]))rownames(vec)<-flatten_string(metadata[["rownames"]])
					}				
				}
			return(vec)
			}
		} else {
		}
	stop("Cannot process ", obj)
	}
	
.onUnload <- function (libpath) {
  library.dynam.unload("RMVL", libpath)
}
