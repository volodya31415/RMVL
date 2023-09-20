require("RMVL")

M3<-mvl_open("test_bracket2a.mvl", append=TRUE, create=TRUE)

L<-list()

df<-data.frame(x=1:1e5, y=rnorm(1e5), s=rep(c("a", "b"), 5e4), b=rnorm(1e5)<0.5)
L[["x"]]<-mvl_write_object(M3, df)

aa<-array(rnorm(10000), c(10, 50, 20))
L[["y"]]<-aa

mm<-matrix(rnorm(10000), 10, 1000)
L[["z"]]<-mm

LL2<-as.list(rnorm(10000))
names(LL2)<-paste("x", 1:10000, sep="")
L[["LL2"]]<-LL2

L[["description"]]<-"Example of large data frame"
mvl_write_object(M3, L, "test_object")

LM1<-lm(rnorm(100)~runif(100))
mvl_write_serialized_object(M3, LM1, "LM1")

mvl_close(M3)


M3<-mvl_open("test_bracket2a.mvl")
print(names(M3))

L2<-M3["test_object", ref=TRUE]

N<-dim(df)[1]

compare_df<-function(x, y) {
	if(length(dim(x))!=length(dim(y)))return(FALSE)
	if(any(dim(x)!=dim(y)))return(FALSE)
	if(any(names(x)!=names(y)))return(FALSE)
	if(dim(x)[2]>0) {
		for(i in 1:(dim(x)[2])) {
			if(any(x[,i]!=y[,i]))return(FALSE)
			}
		}
	return(TRUE)
	}

if(!compare_df(df, mvl2R(L2[["x"]]))) {
	cat("test1a failed\n")
	print(attributes(df))
	print(attributes(mvl2R(L2[["x"]])))
	cat("-----------\n")
	}
	
if(!isTRUE(all.equal(L[c(2, 3)], L2[c(2,3), recurse=TRUE]))) {
	cat("test2a failed\n")
	print(all.equal(L[c(2, 3)], L2[c(2, 3), recurse=TRUE]))
	print(attributes(L[c(2, 3)]))
	print(attributes(L2[c(2, 3), recurse=TRUE]))
	cat("-----------\n")
	}

if(!isTRUE(all.equal(L[c(2, NA, 3)], L2[c(2, NA, 3), recurse=TRUE]))) {
	cat("test2b failed\n")
	print(all.equal(L[c(2, NA, 3)], L2[c(2, NA, 3), recurse=TRUE]))
	print(attributes(L[c(2, NA, 3)]))
	print(attributes(L2[c(2, NA, 3), recurse=TRUE]))
	cat("-----------\n")
	}
	
if(!isTRUE(all.equal(L[c("y", "z")], L2[c("y", "z"), recurse=TRUE]))) {
	cat("test2c failed\n")
	print(all.equal(L[c("y", "z")], L2[c("y", "z"), recurse=TRUE]))
	print(attributes(L[c("y", "z")]))
	print(attributes(L2[c("y", "z"), recurse=TRUE]))
	cat("-----------\n")
	}

if(!isTRUE(all.equal(L[c("W", "y", "z")], L2[c("W", "y", "z"), recurse=TRUE]))) {
	cat("test2d failed\n")
	print(all.equal(L[c("W", "y", "z")], L2[c("W", "y", "z"), recurse=TRUE]))
	print(attributes(L[c("W", "y", "z")]))
	print(attributes(L2[c("W", "y", "z"), recurse=TRUE]))
	cat("-----------\n")
	}

if(!isTRUE(all.equal(L[c("W", "y", NA, "z")], L2[c("W", "y", NA, "z"), recurse=TRUE]))) {
	cat("test2e failed\n")
	print(all.equal(L[c("W", "y", NA, "z")], L2[c("W", "y", NA, "z"), recurse=TRUE]))
	print(attributes(L[c("W", "y", NA, "z")]))
	print(attributes(L2[c("W", "y", NA, "z"), recurse=TRUE]))
	cat("-----------\n")
	}
	
