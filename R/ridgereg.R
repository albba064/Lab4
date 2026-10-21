# TODO add documentation
ridgereg <- function(formula, data, lambda) {
  # TODO input check.

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

  fitted_values <- X %*% coefficients

  model <- list(
    call = match.call(),
    coefficients = coefficients,
    fitted.values = fitted_values,
    formula = formula,
    lambda = lambda,
    x_mean = x_mean,
    x_sd = x_sd
  )

  class(model) <- "ridgereg"
  return(model)
}

print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  invisible(x)
}

predict.ridgereg <- function() {
  # TODO
}

coef.ridgereg <- function() {
  # TODO
}


# Kod för att testa 

data(iris)

model <- ridgereg(
  Petal.Length ~ Sepal.Length + Sepal.Width,
  data = iris,
  lambda = 1
)

# MASS Ridge regression
reference <- MASS::lm.ridge(
  Petal.Length ~ Sepal.Length + Sepal.Width,
  data = iris,
  lambda = 1
)

print(model)
