#' Fit Ridge Regression Using QR Decomposition
#'
#' @description
#' Fits a ridge regression model using a QR decomposition based on
#' modified Gram-Schmidt orthogonalization. The explanatory variables
#' are standardized before fitting, and the estimated coefficients
#' are transformed back to their original scale.
#'
#' @param formula A formula specifying the response and explanatory variables.
#' @param data A data frame containing the variables in the formula.
#' @param lambda A non-negative numeric value specifying the
#'   regularization strength.
#'
#' @return An object of class \code{ridgereg} containing:
#' \describe{
#'   \item{call}{The matched function call.}
#'   \item{coefficients}{Estimated regression coefficients on the original scale.}
#'   \item{fitted.values}{Predicted values for the training data.}
#'   \item{formula}{The model formula.}
#'   \item{lambda}{The regularization parameter.}
#'   \item{x_mean}{Means of the explanatory variables.}
#'   \item{x_sd}{Standard deviations of the explanatory variables.}
#' }
#'
#' @examples
#' model <- ridgereg(
#'   Petal.Length ~ Sepal.Length + Sepal.Width,
#'   data = iris,
#'   lambda = 1
#' )
#'
#' print(model)
#' coef(model)
#'
#' @export
ridgereg <- function(formula, data, lambda) {
  stopifnot(
    "formula must be a formula object." = class(formula) == "formula",
    "formula must contain a response and at least one predictor." = length(formula) == 3,
    "data must be a data.frame" = is.data.frame(data),
    "data cannot be empty" = nrow(data) > 0,
    "lambda must be a numeric" = is.numeric(lambda),
    "lambda must be finite" = is.finite(lambda),
    "lambda must be of lenght one" = length(lambda) == 1,
    "lambda must be a non-negative numeric" = lambda >= 0
  )


  X <- stats::model.matrix(formula, data)
  X_norm <- X
  X_norm[, -1] <- scale(X[, -1])

  y <- data[, all.vars(formula)[1]]

  n <- nrow(X)
  p <- ncol(X)

  P <- diag(p)
  P[1, 1] <- 0

  B <- rbind(X_norm, sqrt(lambda) * P)
  b <- c(y, rep.int(0, p))

  Q <- matrix(0, nrow = n + p, ncol = p)
  R <- matrix(0, nrow = p, ncol = p)

  B_temp <- B

  for (i in 1:p) {
    R[i, i] <- sqrt(sum(B_temp[, i]^2))
    Q[, i] <- B_temp[, i] / R[i, i]

    if (i < p) {
      remaining <- (i + 1):p

      R[i, remaining] <- crossprod(
        Q[, i],
        B_temp[, remaining, drop = FALSE]
      )

      B_temp[, remaining] <- B_temp[, remaining] - outer(
        Q[, i], R[i, remaining]
      )
    }
  }
  beta_hat <- backsolve(R, crossprod(Q, b))

  x_mean <- apply(X[, -1, drop = FALSE], 2, mean)
  x_sd <- apply(X[, -1, drop = FALSE], 2, sd)

  beta_1 <- beta_hat[-1] / x_sd
  beta_0 <- beta_hat[1] - sum(beta_1 * x_mean)
  coefficients <- c(beta_0, beta_1)
  names(coefficients) <- colnames(X)

  fitted_values <- c(X %*% coefficients)

  model <- list(
    call = match.call(),
    coefficients = coefficients,
    fitted.values = fitted_values,
    formula = formula,
    lambda = lambda,
    x_mean = x_mean,
    x_sd = x_sd,
    terms_obj = stats::terms(formula, data = data)
  )

  class(model) <- "ridgereg"
  return(model)
}

#' Print a Ridge Regression Model
#'
#' @param x An object of class `ridgereg`.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns the `ridgereg` object.
#'
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  invisible(x)
}

#' Predict Values Using Ridge Regression
#'
#' @param object An object of class `ridgereg`.
#' @param newdata Optional data frame containing new observations.
#' @param ... Additional arguments (not used).
#'
#' @return A numeric vector of predicted values.
#'
#' @export
predict.ridgereg <- function(object, newdata, ...) {
  if (missing(newdata)) {
    return(object$fitted.values)
  }

  stopifnot(
    "newdata must be a data.frame" = is.data.frame(newdata),
    "newdata cannot be an empty data.frame" = nrow(newdata) > 0
  )

  predictors <- stats::delete.response(object$terms)

  X_new <- stats::model.matrix(predictors, newdata)

  y_new <- X_new %*% object$coefficients
  return(drop(y_new))
}

#' Extract Ridge Regression Coefficients
#'
#' @param object An object of class `ridgereg`.
#' @param ... Additional arguments (not used).
#'
#' @return A named numeric vector of regression coefficients.
#'
#' @export
coef.ridgereg <- function(object, ...) {
  return(object$coefficients)
}
