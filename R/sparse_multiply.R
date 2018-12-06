
#' Sparse matrix implementation
#'
#' @description This code created a sparse.matrix class which has '+', '%*%', and 't' methods.
#' @param i row numbers
#' @param j column numbers
#' @param x non-zero values of the sparse matrix
#' @param dims dimension of the sparse matrix
#' @return A sparse matrix


#Create sparse.matrix class
sparse.matrix <- function(i, j, x, dims=(c(max(i), max(j)))) {
  d <- data.frame(i, j, x)
  structure(list(d, dim=dims), class='sparse.matrix')
}

#Add dispatch
`+.sparse.matrix` <- function(a, b) {
  if (!inherits(b, "sparse.matrix")) {
    stop("Argument b is not a sparse matrix type.")
  }
  if (!identical(a$dim, b$dim)) {
    stop("Dimensions of two objects do not match.")
  }
  c <- merge(a[[1]], b[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2")) 
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")] 
  sparse.matrix(c$i, c$j, c$x, dims=a$dim)
}

#Multiply disptch
`%*%` <- function(a, b) {
  UseMethod("%*%", a)
}
`%*%.sparse.matrix` <- function(a, b) {
  if (!inherits(b, "sparse.matrix")) {
    stop("Argument b is not a sparse matrix type.")
  }
  if (!identical(a$dim[2], b$dim[1])) {
    stop("Dimensions of two objects do not match.")
  }
  
  colnames(b[[1]]) <- c("ib", "jb", "xb")
  c <- merge(a[[1]], b[[1]], by.x="j", by.y="ib", all = TRUE, suffixes = c("1", "2"))
  c$x[is.na(c$x)] <- 0
  c$x <- c$x*c$xb
  c$group <- paste(c$i, c$jb, sep=',')
  c <- c[complete.cases(c),]
  d <- tapply(c$x, c$group, sum)
  index <- strsplit(names(d), split=',')
  i <- as.numeric(sapply(index, getElement, 1))
  j <- as.numeric(sapply(index, getElement, 2))
  x <- as.numeric(d)
  sparse.matrix(i, j, x, dims=c(a$dim[1], b$dim[2]))
}

#Transpose dispatch
`t` <- function(a) {
  UseMethod("t", a)
}
`t.sparse.matrix` <- function(a) {
  dims_t <- c(a$dim[2], a$dim[1])
  sparse.matrix(a[[1]]$j, a[[1]]$i, a[[1]]$x, dims=dims_t)
}
