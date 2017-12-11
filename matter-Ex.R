pkgname <- "matter"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('matter')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("apply")
### * apply

flush(stderr()); flush(stdout())

### Name: apply
### Title: Apply Functions Over "matter" Matrices
### Aliases: apply apply,matter_mat-method
### Keywords: methods

### ** Examples

x <- matter(1:100, nrow=10, ncol=10)

apply(x, 2, summary)



cleanEx()
nameEx("biglm")
### * biglm

flush(stderr()); flush(stdout())

### Name: bigglm
### Title: Using "biglm" with "matter"
### Aliases: biglm biglm,formula,matter_df-method bigglm
###   bigglm,formula,matter_df-method bigglm,formula,matter_mat-method
### Keywords: models regression

### ** Examples

set.seed(1)

x <- matter_mat(rnorm(1000), nrow=100, ncol=10)

colnames(x) <- c(paste0("x", 1:9), "y")

fm <- paste0("y ~ ", paste0(paste0("x", 1:9), collapse=" + "))
fm <- as.formula(fm)

fit <- bigglm(fm, data=x, chunksize=50)
coef(fit)



cleanEx()
nameEx("bsearch")
### * bsearch

flush(stderr()); flush(stdout())

### Name: bsearch
### Title: Binary Search with Approximate Matching
### Aliases: bsearch
### Keywords: utilities

### ** Examples

x <- c(1.11, 2.22, 3.33, 5.0, 5.1)

bsearch(2.22, x) # 2
bsearch(3.0, x) # NA
bsearch(3.0, x, nearest=TRUE) # 3
bsearch(3.0, x, tol=0.1, tol.ref="values") # 3

y <- c("hello", "world!")
bsearch("world!", y) # 2
bsearch("worl", y) # NA
bsearch("worl", y, nearest=TRUE) # 2



cleanEx()
nameEx("checksum")
### * checksum

flush(stderr()); flush(stdout())

### Name: checksum
### Title: Calculate Checksums and Cryptographic Hashes
### Aliases: checksum checksum,matter-method
### Keywords: utilities

### ** Examples

x <- matter(1:10)
y <- matter(1:10)

checksum(x)
checksum(y) # should be the same



cleanEx()
nameEx("combiner-method")
### * combiner-method

flush(stderr()); flush(stdout())

### Name: combiner
### Title: Get or Set combiner for an Object
### Aliases: combiner combiner<-
### Keywords: utilities

### ** Examples

x <- sparse_mat(diag(10))
combiner(x)
combiner(x) <- "sum"
x[]



cleanEx()
nameEx("delayed-ops")
### * delayed-ops

flush(stderr()); flush(stdout())

### Name: delayed-ops
### Title: Delayed Operations on "matter" Objects
### Aliases: Arith Compare Arith,matter_vec,numeric-method
###   Arith,numeric,matter_vec-method Arith,matter_vec,matter_vec-method
###   Arith,numeric,matter_matc-method Arith,matter_matc,numeric-method
###   Arith,matter_matc,matter_matc-method Arith,numeric,matter_matr-method
###   Arith,matter_matr,numeric-method Arith,matter_matr,matter_matr-method
###   Arith,matter_arr,matter_arr-method Arith,matter_arr,numeric-method
###   Arith,numeric,matter_arr-method Arith,matter_fc,numeric-method
###   Arith,numeric,matter_fc-method Arith,matter_fc,matter_fc-method
###   Compare,matter_vec,raw-method Compare,matter_vec,numeric-method
###   Compare,raw,matter_vec-method Compare,numeric,matter_vec-method
###   Compare,matter_vec,matter_vec-method Compare,raw,matter_matc-method
###   Compare,numeric,matter_matc-method Compare,matter_matc,raw-method
###   Compare,matter_matc,numeric-method
###   Compare,matter_matc,matter_matc-method Compare,raw,matter_matr-method
###   Compare,numeric,matter_matr-method Compare,matter_matr,raw-method
###   Compare,matter_matr,numeric-method
###   Compare,matter_matr,matter_matr-method Compare,matter_arr,raw-method
###   Compare,matter_arr,numeric-method Compare,raw,matter_arr-method
###   Compare,numeric,matter_arr-method
###   Compare,matter_arr,matter_arr-method Compare,matter_fc,numeric-method
###   Compare,matter_fc,character-method Compare,matter_fc,factor-method
###   Compare,numeric,matter_fc-method Compare,character,matter_fc-method
###   Compare,factor,matter_fc-method Compare,matter_fc,matter_fc-method
###   exp,matter_vec-method exp,matter_mat-method exp,matter_arr-method
###   exp,matter_fc-method log,matter_vec-method log,matter_matc-method
###   log,matter_matr-method log,matter_arr-method log,matter_fc-method
###   log,matter_vec,numeric-method log,matter_matc,numeric-method
###   log,matter_matr,numeric-method log,matter_arr,numeric-method
###   log,matter_fc,numeric-method log2,matter_vec-method
###   log2,matter_mat-method log2,matter_arr-method log2,matter_fc-method
###   log10,matter_vec-method log10,matter_mat-method
###   log10,matter_arr-method log10,matter_fc-method
### Keywords: methods arith

### ** Examples

x <- matter(1:100)
y <- 2 * x + 1

x[1:10]
y[1:10]

mean(x)
mean(y)



cleanEx()
nameEx("drle-class")
### * drle-class

flush(stderr()); flush(stdout())

### Name: drle-class
### Title: Delta Run Length Encoding
### Aliases: class:drle drle drle-class
###   [,drle,missing,missing,missing-method
###   [,drle,ANY,missing,missing-method c,drle-method length,drle-method
###   as.vector,drle-method as.list,drle-method is.drle
### Keywords: classes

### ** Examples

## Create a drle vector
x <- c(1,1,1,1,1,6,7,8,9,10,21,32,33,34,15)
y <- drle(x)

# Check that their elements are equal
x == y[]



cleanEx()
nameEx("keys-method")
### * keys-method

flush(stderr()); flush(stdout())

### Name: keys
### Title: Get or Set Keys for an Object
### Aliases: keys keys<-
### Keywords: utilities

### ** Examples

x <- sparse_mat(diag(10))
keys(x)
keys(x) <- 1:10
x[]



cleanEx()
nameEx("matter-class")
### * matter-class

flush(stderr()); flush(stdout())

### Name: matter-class
### Title: Vectors, Matrices, and Arrays Stored on Disk
### Aliases: class:matter matter matter-class adata adata,matter-method
###   atomdata atomdata,matter-method atomdata<- atomdata<-,matter-method
###   datamode datamode<- datamode,atoms-method datamode<-,atoms-method
###   datamode,matter-method datamode<-,matter-method
###   datamode<-,matter_vt-method paths paths<- paths,matter-method
###   paths<-,matter-method paths<-,matter_vt-method filemode filemode<-
###   filemode,matter-method filemode<-,matter-method
###   filemode<-,matter_vt-method readonly readonly<-
###   readonly,matter-method readonly<-,matter-method
###   readonly<-,matter_vt-method chunksize chunksize<-
###   chunksize,matter-method chunksize<-,matter-method
###   chunksize<-,matter_vt-method length,atoms-method length,matter-method
###   length<-,matter-method dim,atoms-method dim,matter-method
###   dim<-,matter-method names,matter-method names<-,matter-method
###   dimnames,matter-method dimnames<-,matter,ANY-method
###   [,atoms,ANY,missing,ANY-method [,atoms,missing,ANY,ANY-method
###   [,atoms,ANY,ANY,ANY-method [[,atoms-method [[,atoms,ANY,ANY-method
###   c,atoms-method c,matter-method which,matter-method is.matter
###   as.matter
### Keywords: classes array IO

### ** Examples

## Create a matter_vec vector
x <- matter(1:100, length=100)
x[]

## Create a matter_mat matrix
x <- matter(1:100, nrow=10, ncol=10)
x[]



cleanEx()
nameEx("matter_arr-class")
### * matter_arr-class

flush(stderr()); flush(stdout())

### Name: matter_arr-class
### Title: Arrays Stored on Disk
### Aliases: class:matter_arr matter_arr matter_arr-class
###   [,matter_arr-method [,matter_arr,ANY,ANY,ANY-method
###   [<-,matter_arr-method [<-,matter_arr,ANY,ANY,ANY-method
###   dim<-,matter_arr-method as.vector,matter_arr-method
###   as.array,matter_arr-method
### Keywords: classes array IO

### ** Examples

x <- matter_arr(1:125, dim=c(5,5,5))
x[]



cleanEx()
nameEx("matter_df-class")
### * matter_df-class

flush(stderr()); flush(stdout())

### Name: matter_df-class
### Title: Data Frames Stored on Disk
### Aliases: class:matter_df matter_df matter_df-class
###   names<-,matter_tbl-method dimnames<-,matter_tbl,ANY-method
###   [,matter_df-method [,matter_df,ANY,ANY,ANY-method
###   [,matter_df,ANY,ANY,NULL-method [,matter_df,ANY,missing,ANY-method
###   [,matter_df,ANY,missing,NULL-method
###   [,matter_df,missing,ANY,ANY-method
###   [,matter_df,missing,ANY,NULL-method
###   [,matter_df,missing,missing,ANY-method [<-,matter_df-method
###   [<-,matter_df,ANY,ANY,ANY-method [<-,matter_df,ANY,missing,ANY-method
###   [<-,matter_df,missing,ANY,ANY-method
###   [<-,matter_df,missing,missing,ANY-method
###   [[,matter_df,ANY,missing-method [[<-,matter_df,ANY,missing-method
###   $,matter_df-method $<-,matter_df-method head,matter_tbl-method
###   tail,matter_tbl-method as.data.frame,matter_df-method
### Keywords: classes array IO

### ** Examples

x <- matter_df(a=as.matter(1:10), b=as.matter(1:10))
x[]
x[[1]]
x[["a"]]
x[,"a"]
x[1:5,c("a","b")]
x$a
x$a[1:10]



cleanEx()
nameEx("matter_fc-class")
### * matter_fc-class

flush(stderr()); flush(stdout())

### Name: matter_fc-class
### Title: Factors Stored on Disk
### Aliases: class:matter_fc matter_fc matter_fc-class [,matter_fc-method
###   [,matter_fc,ANY,missing,ANY-method
###   [,matter_fc,ANY,missing,NULL-method
###   [,matter_fc,missing,missing,ANY-method [<-,matter_fc-method
###   [<-,matter_fc,ANY,missing,ANY-method
###   [<-,matter_fc,missing,missing,ANY-method levels,matter_fc-method
###   levels<-,matter_fc-method
### Keywords: classes array IO

### ** Examples

x <- matter_fc(c("a", "a", "b"), levels=c("a", "b", "c"))
x[]



cleanEx()
nameEx("matter_list-class")
### * matter_list-class

flush(stderr()); flush(stdout())

### Name: matter_list-class
### Title: Lists of Vectors Stored on Disk
### Aliases: class:matter_list matter_list matter_list-class
###   lengths,matter_list-method [,matter_list-method
###   [,matter_list,ANY,ANY,ANY-method [,matter_list,ANY,ANY,NULL-method
###   [,matter_list,ANY,missing,ANY-method
###   [,matter_list,ANY,missing,NULL-method
###   [,matter_list,missing,missing,ANY-method [<-,matter_list-method
###   [<-,matter_list,ANY,ANY,ANY-method
###   [<-,matter_list,ANY,missing,ANY-method
###   [<-,matter_list,missing,missing,ANY-method
###   [[,matter_list,ANY,missing-method [[<-,matter_list,ANY,missing-method
###   $,matter_list-method $<-,matter_list-method
###   as.list,matter_list-method
### Keywords: classes array IO

### ** Examples

x <- matter_list(list(c(TRUE,FALSE), 1:5, c(1.11, 2.22, 3.33)), lengths=c(2,5,3))
x[]
x[[1]]
x[3,2]
x[2,5]



cleanEx()
nameEx("matter_mat-class")
### * matter_mat-class

flush(stderr()); flush(stdout())

### Name: matter_mat-class
### Title: Matrices Stored on Disk
### Aliases: class:matter_mat matter_mat matter_matc matter_matr
###   matter_mat-class matter_matc-class matter_matr-class
###   [,matter_mat-method [,matter_mat,ANY,ANY,ANY-method
###   [,matter_mat,ANY,ANY,NULL-method [,matter_mat,ANY,missing,ANY-method
###   [,matter_mat,ANY,missing,NULL-method
###   [,matter_mat,missing,ANY,ANY-method
###   [,matter_mat,missing,ANY,NULL-method
###   [,matter_mat,missing,missing,ANY-method [<-,matter_mat-method
###   [<-,matter_mat,ANY,ANY,ANY-method
###   [<-,matter_mat,ANY,missing,ANY-method
###   [<-,matter_mat,missing,ANY,ANY-method
###   [<-,matter_mat,missing,missing,ANY-method cbind,matter-method
###   rbind,matter-method t.matter t,matter_matc-method
###   t,matter_matr-method %*%,matter,matter-method
###   %*%,matrix,matter_mat-method %*%,matter_mat,matrix-method
###   %*%,matter_matc,numeric-method %*%,matter_matr,numeric-method
###   %*%,numeric,matter_matc-method %*%,numeric,matter_matr-method
###   crossprod,matter,ANY-method crossprod,ANY,matter-method
###   tcrossprod,matter,ANY-method tcrossprod,ANY,matter-method
###   as.matrix,matter_mat-method
### Keywords: classes array IO

### ** Examples

x <- matter_mat(1:100, nrow=10, ncol=10)
x[]



cleanEx()
nameEx("matter_str-class")
### * matter_str-class

flush(stderr()); flush(stdout())

### Name: matter_str-class
### Title: Strings Stored on Disk
### Aliases: class:matter_str matter_str matter_str-class
###   lengths,matter_str-method [,matter_str-method
###   [,matter_str,ANY,ANY,ANY-method [,matter_str,ANY,ANY,NULL-method
###   [,matter_str,ANY,missing,ANY-method
###   [,matter_str,ANY,missing,NULL-method
###   [,matter_str,missing,missing,ANY-method [<-,matter_str-method
###   [<-,matter_str,ANY,ANY,ANY-method
###   [<-,matter_str,ANY,missing,ANY-method
###   [<-,matter_str,missing,missing,ANY-method
###   [[,matter_str,ANY,missing-method [[<-,matter_str,ANY,missing-method
###   as.vector,matter_str-method
### Keywords: classes array IO

### ** Examples

x <- matter_str(c("hello", "world!"))
x[]



cleanEx()
nameEx("matter_vec-class")
### * matter_vec-class

flush(stderr()); flush(stdout())

### Name: matter_vec-class
### Title: Vectors Stored on Disk
### Aliases: class:matter_vec matter_vec matter_vec-class
###   [,matter_vec-method [,matter_vec,ANY,missing,ANY-method
###   [,matter_vec,ANY,missing,NULL-method
###   [,matter_vec,missing,missing,ANY-method [<-,matter_vec-method
###   [<-,matter_vec,ANY,missing,ANY-method
###   [<-,matter_vec,missing,missing,ANY-method c,matter_vec-method
###   t,matter_vec-method dim<-,matter_vec-method
###   as.vector,matter_vec-method as.matrix,matter_vec-method
###   as.array,matter_vec-method
### Keywords: classes array IO

### ** Examples

x <- matter_vec(1:100)
x[]



cleanEx()
nameEx("prcomp")
### * prcomp

flush(stderr()); flush(stdout())

### Name: prcomp
### Title: Principal Components Analysis for "matter" Matrices
### Aliases: prcomp prcomp,matter_mat-method
### Keywords: multivariate

### ** Examples

set.seed(1)

x <- matter_mat(rnorm(1000), nrow=100, ncol=10)

prcomp(x)



cleanEx()
nameEx("profmem")
### * profmem

flush(stderr()); flush(stdout())

### Name: profmem
### Title: Profile Memory Use
### Aliases: profmem mem
### Keywords: utilities

### ** Examples

x <- 1:100

mem(x)

profmem(mean(x + 1))



cleanEx()
nameEx("scale")
### * scale

flush(stderr()); flush(stdout())

### Name: scale
### Title: Scaling and Centering of "matter" Matrices
### Aliases: scale scale.matter scale,matter_mat-method
### Keywords: methods

### ** Examples

x <- matter(1:100, nrow=10, ncol=10)

scale(x)



cleanEx()
nameEx("sparse_mat-class")
### * sparse_mat-class

flush(stderr()); flush(stdout())

### Name: sparse_mat-class
### Title: Sparse Matrices
### Aliases: class:sparse_mat sparse_mat sparse_matc sparse_matr
###   sparse_mat-class sparse_matc-class sparse_matr-class
###   keys,sparse_mat-method keys<-,sparse_mat-method
###   tolerance,sparse_mat-method tolerance<-,sparse_mat-method
###   combiner,sparse_mat-method combiner<-,sparse_mat-method
###   datamode<-,sparse_mat-method [,sparse_mat-method
###   [,sparse_mat,ANY,ANY,ANY-method [,sparse_mat,ANY,ANY,NULL-method
###   [,sparse_mat,ANY,missing,ANY-method
###   [,sparse_mat,ANY,missing,NULL-method
###   [,sparse_mat,missing,ANY,ANY-method
###   [,sparse_mat,missing,ANY,NULL-method
###   [,sparse_mat,missing,missing,ANY-method [<-,sparse_mat-method
###   [<-,sparse_mat,ANY,ANY,ANY-method
###   [<-,sparse_mat,ANY,missing,ANY-method
###   [<-,sparse_mat,missing,ANY,ANY-method
###   [<-,sparse_mat,missing,missing,ANY-method t,sparse_matc-method
###   t,sparse_matr-method is.sparse as.sparse
### Keywords: classes array

### ** Examples

keys <- list(
    c(1,4,8,10),
    c(2,3,5),
    c(1,2,7,9))

values <- list(
    rnorm(4),
    rnorm(3),
    rnorm(4))

init1 <- list(keys=keys, values=values)

x <- sparse_mat(init1, nrow=10)
x[]

init2 <- matrix(rbinom(100, 1, 0.2), nrow=10, ncol=10)

y <- sparse_mat(init2, keys=letters[1:10])
y[]



cleanEx()
nameEx("struct")
### * struct

flush(stderr()); flush(stdout())

### Name: struct
### Title: C-Style Structs Stored on Disk
### Aliases: struct
### Keywords: utilities array IO

### ** Examples

x <- struct(first=c(int=1), second=c(double=1))

x$first <- 2L
x$second <- 3.33

x$first
x$second



cleanEx()
nameEx("summary-stats")
### * summary-stats

flush(stderr()); flush(stdout())

### Name: summary-stats
### Title: Summary Statistics for "matter" Objects
### Aliases: Summary mean sd var range,matter-method min,matter-method
###   max,matter-method prod,matter-method sum,matter-method
###   mean,matter-method sd,matter-method var,matter-method
###   any,matter-method all,matter-method colSds colVars
###   colMeans,matter_mat-method colSums,matter_mat-method
###   colSds,matter_mat-method colVars,matter_mat-method rowSds rowVars
###   rowMeans,matter_mat-method rowSums,matter_mat-method
###   rowSds,matter_mat-method rowVars,matter_mat-method
### Keywords: methods univar

### ** Examples

x <- matter(1:100, nrow=10, ncol=10)

sum(x)
mean(x)
var(x)
sd(x)

colSums(x)
colMeans(x)
colVars(x)
colSds(x)

rowSums(x)
rowMeans(x)
rowVars(x)
rowSds(x)



cleanEx()
nameEx("tolerance-method")
### * tolerance-method

flush(stderr()); flush(stdout())

### Name: tolerance
### Title: Get or Set Tolerance for an Object
### Aliases: tolerance tolerance<-
### Keywords: utilities

### ** Examples

x <- sparse_mat(diag(10), keys=rnorm(10))
tolerance(x)
tolerance(x) <- c(absolute=0.1)
x[]



cleanEx()
nameEx("uuid")
### * uuid

flush(stderr()); flush(stdout())

### Name: uuid
### Title: Universally Unique Identifiers
### Aliases: uuid raw2hex hex2raw
### Keywords: utilities

### ** Examples

id <- uuid()
id
hex2raw(id$string)
raw2hex(id$bytes)



cleanEx()
nameEx("virtual_mat-class")
### * virtual_mat-class

flush(stderr()); flush(stdout())

### Name: virtual_mat-class
### Title: Virtual Matrices
### Aliases: class:virtual_mat virtual_mat virtual_matc virtual_matr
###   virtual_mat-class virtual_matc-class virtual_matr-class
###   [,virtual_mat-method [,virtual_mat,ANY,ANY,ANY-method
###   [,virtual_mat,ANY,ANY,NULL-method
###   [,virtual_mat,ANY,missing,ANY-method
###   [,virtual_mat,ANY,missing,NULL-method
###   [,virtual_mat,missing,ANY,ANY-method
###   [,virtual_mat,missing,ANY,NULL-method
###   [,virtual_mat,missing,missing,ANY-method
###   datamode<-,virtual_mat-method t,virtual_mat-method is.virtual
###   as.virtual
### Keywords: classes array

### ** Examples

x <- matrix(runif(50), nrow=10, ncol=5)

x <- virtual_mat(list(x, x))
x[]



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
