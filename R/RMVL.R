mvl_open<-function(filename, append=FALSE, create=FALSE) {
	cat("test\n")
	MVLHANDLE<-list(handle=.Call("mmap_library", as.character(filename), as.integer(ifelse(append, 1, 0)+ifelse(create, 2, 0))))
	class(MVLHANDLE)<-"MVL"
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

mvl_get_vectors<-function(MVLHANDLE, offsets) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!inherits(offsets, "MVL_OFFSET"))stop("not an MVL offset")
	return(.Call("read_vectors", MVLHANDLE[["handle"]], offsets))
	}
	
mvl_write_vector<-function(MVLHANDLE, x, metadata.offset=NULL) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!is.null(metadata.offset) && !inherits(metadata.offset, "MVL_OFFSET"))stop("not an MVL offset")
	if(class(x)=="factor")x<-as.character(x)
	type<-switch(class(x), numeric=5, integer=2, MVL_OFFSET=100, character=10000, -1)
	if(type>0) {
		return(.Call("write_vector", MVLHANDLE[["handle"]], as.integer(type), x, metadata.offset)) 
		}
	stop("Could not write vector")
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
	if(!is.null(class(x))) {
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

mvl_write_object<-function(MVLHANDLE, x) {
	cat("Writing", class(x), typeof(x), "\n")
	metadata<-mvl_write_object_metadata(MVLHANDLE, x)
	if(class(x) %in% c("numeric", "character", "integer", "factor")) {
		return(mvl_write_vector(MVLHANDLE, x, metadata))
		}
	if(class(x) %in% c("list", "data.frame")) {
		v<-unlist(lapply(x, function(x){return(mvl_write_object(MVLHANDLE, x))}))
		class(v)<-"MVL_OFFSET"
		return(mvl_write_vector(MVLHANDLE, v, metadata))
		}
	stop("Could not write object")
	}
	
	
mvl_read_object<-function(MVLHANDLE, offset) {
	if(!inherits(MVLHANDLE, "MVL")) stop("not an MVL object")
	if(!inherits(offset, "MVL_OFFSET"))stop("not an MVL offset")
	if(offset==0)return(NULL)
	metadata_offset<-.Call("read_metadata", MVLHANDLE[["handle"]], offset)
	metadata<-mvl_read_object(MVLHANDLE, metadata_offset)
	if(!is.null(metadata)) {
		n<-metadata[1:(length(metadata)/2)]
		for(i in 1:length(n)) {
			n[[i]]<-rawToChar(metadata[[i]])
			}
		metadata<-metadata[(length(metadata)/2+1):length(metadata)]
		for(i in 1:length(metadata)) {
			if(class(metadata[[i]])=="raw")metadata[[i]]<-rawToChar(metadata[[i]])
			}
		names(metadata)<-unlist(n)
		}
	vec<-.Call("read_vectors", MVLHANDLE[["handle"]], offset)[[1]]
	if(inherits(vec, "MVL_OFFSET")) {
		vec<-lapply(vec, function(x){class(x)<-"MVL_OFFSET" ; return(mvl_read_object(MVLHANDLE, x))})
		}
#	attr(vec, "metadata")<-metadata
	if(any(metadata[["MVL_LAYOUT"]]=="R")) {
		cl<-metadata[["class"]]
		if(cl=="factor" || cl=="character") {
			vec<-unlist(lapply(vec, rawToChar))
			if(cl=="factor")vec<-as.factor(vec)
			} else 
			class(vec)<-cl
		if(!is.null(metadata[["names"]]))names(vec)<-unlist(lapply(metadata[["names"]], rawToChar))
		if(!is.null(metadata[["rownames"]]))rownames(vec)<-unlist(lapply(metadata[["rownames"]], rawToChar))
		if(cl!="data.frame" && !is.null(metadata[["dim"]]))dim(vec)<-metadata[["dim"]]
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
	
.onUnload <- function (libpath) {
  library.dynam.unload("RlibMVL", libpath)
}
