
linreg <- function(formula, data){

  X <- model.matrix(formula, data)
  y <- data[, all.vars(formula)[1]]

  n <- nrow(X)
  p <- ncol(X)

  #-----------------------------------------------
  # QR-decomposition, using Gram-Schmidt
  #-----------------------------------------------
  X_temp <- X  # Temporary copy of X which will be updated by the algorithm
  Q <- matrix(0, nrow = n, ncol = p)
  R <- matrix(0, nrow = p, ncol = p)

  for (j in 1:p) {

    # Length of current column
    R[j, j] <- sqrt(sum(X_temp[, j]^2))

    # Normalize current column
    Q[, j] <- X_temp[, j] / R[j, j]

    if (j < p) {
      for (k in (j + 1):p) {

        # Projection of column k onto Q[, j]
        R[j, k] <- sum(Q[, j] * X_temp[, k])

        # Remove projection
        X_temp[, k] <- X_temp[, k] - Q[, j] * R[j, k]
      }
    }
  }

  #-----------------------------------------------
  # Calculate regression coefficients w. QR
  #-----------------------------------------------
  beta_hat <- drop(solve(R) %*% t(Q) %*% y)
  #-----------------------------------------------

  # Fitted values
  fitted_values <- X %*% beta_hat

  # Residuals
  residuals <- y - fitted_values

  # Degrees of freedom df
  df <- n - p

  # Residual variance
  sigma_squared <- sum(residuals^2) / df

  #-----------------------------------------------
  # Vcov. matrix of beta_hat
  #-----------------------------------------------
  var_beta_hat <- sigma_squared * solve(R) %*% t(solve(R))
  #-----------------------------------------------

  # Variance of regrission coefficients
  var_coefficients <- diag(var_beta_hat)

  # Standard errors
  standard_errors <- sqrt(var_coefficients)

  # t-values
  t_values <- beta_hat / standard_errors

  # p-values
  p_values <- 2 * pt(-abs(t_values), df = df)



  # Create linreg object
  result <- list(
    call = match.call(),
    coefficients = beta_hat,
    fitted.values = fitted_values,
    residuals = residuals,
    df = df,
    sigma_squared = sigma_squared,
    var_beta_hat = var_beta_hat,
    var_coefficients = var_coefficients,
    standard_errors = standard_errors,
    t_values = t_values,
    p_values = p_values
  )

  class(result) <- "linreg"

  return(result)


}

print.linreg <- function(x, ...){
  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(x$coefficients)

  invisible(x)

}
#
summary.linreg <- function(object, ...){
  coefficients <- object$coefficients
  standard_error <- sqrt(object$var_coefficients)
  t_value <- object$t_values
  p_value <- object$p_value

  # Print the function call
  cat("Call:\n")
  print(object$call)

  cat("\nCoefficients:\n")
  # Create a table for coefficients, similar to summary.lm() output
  coef_tab <- cbind(
    Estimate = coefficients,
    'Std. Error' = standard_error,
    't value' = t_value,
    'Pr(>|t|)' = p_value
  )
  print(coef_tab, digits=6)

  cat("\nResidual standard error:", format(sqrt(object$residual_variance), digits=4), "on", object$df, "degrees of freedom")
  # result <- list(
  #   call = object$call,
  #   coefficients = coef_tab,
  #   df = object$df,
  #   residual_variance = object$residual_variance
  # )
  # class(result) <- "summary.linreg"
  # return(result)

}

# Create pred.linreg
pred <- function(object, ...){
  UseMethod("pred")
}
pred.linreg <- function(object){
  result <- drop(object$fitted.values)
  class(result) <- "pred"
  return(result)

}

# function returns coefficients as named vector
coef.linreg <- function(object, ...){
  return(object$coefficients)
}

# function returns residual vector
resid.linreg <- function(object, ...){
  return(drop(object$residuals))
}


# fix plot method
plot.linreg <- function(object, ...){
  # fix ggplot2 load
  library(ggplot2)
  data <- data.frame(fitted.values = object$fitted.values, residuals = object$residuals, medi = median(object$residuals))
  smooth_line = lowess()
  ggplot(data, aes(x = fitted.values, y = residuals)) +
    geom_point() +
    geom_smooth(method = "lowess", group = 1) +
    geom_hline(yintercept = 0, linetype = "dashed")
    # + ylim(min(data$residuals), max(data$residuals))
}
test <- linreg(Petal.Length~Species, iris)

data = data.frame(x = test$fitted.values, y = test$residuals)
print(test)
# pred(test)
# coef(test)
resid(test)
plot(test)
summary(test)
library(ggplot2)
ggplot(data = data, aes(x = x, y = y)) + geom_point() + geom_smooth(formula = )
plot(test, 1)
lm_test <- lm(Petal.Length~Species, iris)
lm_test$residuals |> summary()

par(mfrow = c(1, 1))
plot(lm_test, 1)
lines(lowess(lm_test$fitted.values, lm_test$residuals), col = "blue", lt = 2)

plot(lm_test$fitted.values, lm_test$residuals)
lines(lowess(lm_test$fitted.values, lm_test$residuals), col = "red")
abline(h = 0, lty = 3, col = "gray")

lowess()
