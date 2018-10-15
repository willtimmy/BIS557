
#' Fit a ridge regression
#'
#' @description This function passes parameters to the ridge_reg function.
#' @param formula a formula
#' @param lambda a penalty coefficient
#' @param data a data.frame
#' @return An ridge_reg object
#' @import stats 
#' @export

ridge_reg <- function(formula, lambda, data){
  rownames(data) <- NULL 
  #B/c pima already has rownames, since training set and test set does not match the rownames and returns error
  m <- model.matrix(formula, data)
  y <- matrix(data[, as.character(formula)[2]], ncol = 1)
  y <- y[as.numeric(rownames(m)), , drop = FALSE]
 
  #Via SVD:
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  ret <- list(coefficients = beta, lambda = lambda, formula = formula)
  class(ret) <- "ridge_reg"
  ret
}
