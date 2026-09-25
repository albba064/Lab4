
linreg <- function(formula, data){
  matrix_X <- model.matrix(formula, data)
  dependent_Y <- data[,all.vars(formula)[1]]

  # Regressions coefficients
  coef <- drop(solve(crossprod(matrix_X)) %*% t(matrix_X) %*% dependent_Y)

  # Fitted values
  y_fit <- matrix_X %*% coef

  # Residuals
  res <- dependent_Y - y_fit

  # Degrees of freedom df
  df <- length(dependent_Y) - ncol(matrix_X)

  # Residual variance
  residual_variance <- (crossprod(res) / df)[[1]]

  # Variance of the regression coefficients
  var_reg_coef <- residual_variance * diag(solve(crossprod(matrix_X)))

  # t-values for each coefficient
  t_val_reg_coef <-  coef / sqrt(var_reg_coef)

  # p-values for each coefficient
  p_val_reg_coef <- 2 * pt(-abs(t_val_reg_coef), df = df)

  # Create linreg object
  result <- list(
    call = match.call(),
    coefficients = coef,
    fitted.values = y_fit,
    residuals = res,
    df = df,
    residual_variance = residual_variance,
    var_coefficients = var_reg_coef,
    t_values = t_val_reg_coef,
    p_values = p_val_reg_coef
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

  ggplot(data, aes(x = fitted.values, y = residuals, z = medi)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ylim(min(data$residuals), max(data$residuals))
}
test <- linreg(Petal.Length~Species, iris)


# print(test)
# pred(test)
# coef(test)
# resid(test)
# plot(test)
