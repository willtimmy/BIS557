
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
  X <- data.frame(mat[,-1])
  colnames(X) <- names(mat[-1])
    
  beta <- solve((t(X) %*% X)) %*% t(X) %*% Y
  table <- cbind (beta)
  colnames(table)[1] <- 'Estimate'
}


