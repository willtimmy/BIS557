
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.frame
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  #Get X and Y matrix
  mat <- model.frame(formula, data)
  Y <- data.frame(mat[,1])
  colnames(Y) <- names(mat[1])
  X <- model.matrix(formula, data)
  
  #Calculate coefficients
  result <- list()
  X_d <- qr(X)
  result$coefficients <- qr.coef(X_d, Y)
  class(result) <- 'lm'
  result
}


