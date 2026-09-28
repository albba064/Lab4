linreg <- function(formula, data) {
  matrix_X <- model.matrix(formula, data)
  dependent_Y <- data[, all.vars(formula)[1]]

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
  t_val_reg_coef <- coef / sqrt(var_reg_coef)

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

print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(x$coefficients)

  invisible(x)
}

summary.linreg <- function(object, ...) {
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
    "Std. Error" = standard_error,
    "t value" = t_value,
    "Pr(>|t|)" = p_value
  )
  print(coef_tab, digits = 6)

  cat("\nResidual standard error:", format(sqrt(object$residual_variance), digits = 4), "on", object$df, "degrees of freedom")
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
pred <- function(object, ...) {
  UseMethod("pred")
}
pred.linreg <- function(object) {
  result <- drop(object$fitted.values)
  class(result) <- "pred"
  return(result)
}

# function returns coefficients as named vector
coef.linreg <- function(object, ...) {
  return(object$coefficients)
}

# function returns residual vector
resid.linreg <- function(object, ...) {
  return(drop(object$residuals))
}

#' Plot diagnostic plots for a linreg object
#'
#' Creates diagnostic plots for an object of class \code{linreg}.
#' The function produces a Residuals vs Fitted plot and a
#' Scale-Location plot using \code{ggplot2}.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Aditional arguments. Currently not used.
#'
#' @examples
#' model <- linreg(Petal.Length ~ Species, data = iris)
#' plot(model)
#'
#' @export
plot.linreg <- function(object, ...) {
  std_resids <- object$residuals /
    sqrt(object$residual_variance)

  scale_resids <- sqrt(abs(std_resids))

  data <- data.frame(
    fitted.values = object$fitted.values,
    residuals = object$residuals,
    resid_var = object$residual_variance,
    scale_resids = scale_resids
  )

  smooth_line <- lowess(
    data$fitted.values,
    data$residuals
  )

  smooth_line_std_resids <- lowess(
    data$fitted.values,
    data$scale_resids
  )

  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = fitted.values, y = residuals)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = data.frame(
        fitted.values = smooth_line$x,
        residuals = smooth_line$y
      ),
      color = "red"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    )

  p2 <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = fitted.values, y = scale_resids)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = data.frame(
        fitted.values = smooth_line_std_resids$x,
        scale_resids = smooth_line_std_resids$y
      ),
      color = "red"
    )

  print(p1)
  print(p2)
}
