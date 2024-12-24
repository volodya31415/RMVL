require("RMVL")

M<-mvl_open("test_checksums1.mvl", append=TRUE, create=TRUE)

mvl_write_object(M, rnorm(10000), "x")

mvl_close(M, checksums=TRUE)


M<-mvl_open("test_checksums1.mvl")
mvl_verify(M)
mvl_close(M)

unlink("test_checksums1.mvl")
