#' Linear regression model
#'
#' Fits a linear regression model where the regression coefficients and their variance
#' are calculated using a QR decomposition of the design matrix.
#'
#' @param formula A formula describing the regression model.
#' @param data A data frame containing the variables used in the model.
#'
#' @return An object of class \code{linreg} containing the following elements:
#' \describe{
#' \item{call}{The function call used to fit the model.}
#' \item{coefficients}{A named vector containing the estimated regression coefficients.}
#' \item{fitted.values}{A vector containing the fitted values.}
#' \item{residuals}{A vector containing the residuals.}
#' \item{df}{The residual degrees of freedom.}
#' \item{sigma_squared}{The estimated residual variance.}
#' \item{var_beta_hat}{The variance-covariance matrix of the estimated regression coefficients.}
#' \item{var_coefficients}{The estimated variance of each regression coefficient.}
#' \item{standard_errors}{The standard errors of the estimated regression coefficients.}
#' \item{t_values}{The t-statistics for the regression coefficients.}
#' \item{p_values}{The two-sided p-values for the regression coefficients.}
#' }
#'
#' @import stats
#'
#' @export
linreg <- function(formula, data) {
  X <- stats::model.matrix(formula, data)
  y <- data[, all.vars(formula)[1]]

  n <- nrow(X)
  p <- ncol(X)

  #-----------------------------------------------
  # QR-decomposition, using Gram-Schmidt
  #-----------------------------------------------
  X_temp <- X # Temporary copy of X which will be updated by the algorithm
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
  names(beta_hat) <- colnames(X)
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
  p_values <- 2 * stats::pt(-abs(t_values), df = df)


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

#' Print a linreg model
#'
#' Prints the function call and estimated regression
#' coefficients from a \code{linreg} object.
#'
#' @param x An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return The \code{linreg} object
#'
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(x$coefficients)

  invisible(x)
}

#' Extract residuals
#'
#' Extracts the residuals from a \code{linreg} object.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing the residuals.
#'
#' @export
resid.linreg <- function(object, ...) {
  return(object$residuals)
}


#' Extract predicted values
#'
#' Generic function for extracting predicted values from a model.
#'
#' @param object A model object.
#' @param ... Additional arguments.
#'
#' @return The predicted values.
#'
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Extract predicted values from a linreg model
#'
#' Extracts the fitted values from a \code{linreg} object.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing the fitted values, with class
#'   \code{pred}.
#'
#' @export
pred.linreg <- function(object, ...) {
  object$fitted.values
}

#' Extract regression coefficients
#'
#' Extracts the estimated regression coefficients from a \code{linreg} object.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return A named numeric vector containing the estimated regression
#' coefficients.
#' @export
coef.linreg <- function(object, ...) {
  object$coefficients
}


#' Summarize a linreg model
#'
#' Prints a summary of a \code{linreg} object, including the
#' estimated regression coefficients, standard errors,
#' t-statistics, p-values, residual standard error, and residual
#' degrees of freedom.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return The \code{linreg} object, invisibly.
#'
#' @import stats
#' @export

summary.linreg <- function(object, ...) {
  # Print the function call
  cat("Call:\n")
  print(object$call)

  cat("\nCoefficients:\n")
  # Create a table for coefficients, similar to summary.lm() output
  coef_tab <- cbind(
    Estimate = object$coefficients,
    "Std. Error" = object$standard_errors,
    "t value" = object$t_value,
    "Pr(>|t|)" = object$p_value
  )
  stats::printCoefmat(coef_tab, digits = 6)

  cat(
    "\nResidual standard error:",
    format(sqrt(object$sigma_squared), digits = 4),
    "on",
    object$df,
    "degrees of freedom"
  )
}

plot <- function(object, ...) {
  UseMethod("plot")
}

#' Plot diagnostic plots for a linreg object
#'
#' Creates diagnostic plots for an object of class \code{linreg}.
#' The function produces a Residuals vs Fitted plot and a
#' Scale-Location plot using \code{ggplot2}.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @import ggplot2
#' @import stats
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

  smooth_line <- stats::lowess(
    data$fitted.values,
    data$residuals
  )

  smooth_line_std_resids <- stats::lowess(
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
