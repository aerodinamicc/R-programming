#large and structured datasets
#hierarchical data format
# www.hdfgoup.org

#loading the biocLite function #packages used for genomics but also has good big data packages
#it could be used to interface with hdf5 data sets
# http://www.bioconductor.org/packages/release/bioc/vignettes/rhdf/inst/doc/rhdf5.pdf
setwd("E:/R/Coursera/GCD/Week 2")
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
created = h5createFile("example.h5")
#Create groups
created = h5createGroup("example.h5", "foo")
created = h5createGroup("example.h5", "baa")
created = h5createGroup("example.h5", "foo/foobaa")
h5ls("example.h5")
#Write to groups
A=matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A") # what/to/where
B = array(seq(0.1,2.0,by=0.1), dim=c(5,2,2)) #multidimensional array
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
h5ls("example.h5")
h5read()

