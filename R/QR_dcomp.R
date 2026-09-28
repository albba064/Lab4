
#mat <- matrix(c(1, 0, 2, 0, 2, 0, 0, -1, 1), ncol=3, byrow = T)

mat <- model.matrix(Petal.Length ~ Species, iris)

Q <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
R <- matrix(0, nrow = ncol(mat), ncol = ncol(mat))

for(j in 1:ncol(mat)){

  # Length of current column
  R[j, j] <- sqrt(sum(mat[, j]^2))

  # Normalize current column
  Q[, j] <- mat[, j] / R[j, j]

  if(j < ncol(mat)){

    for(k in (j + 1):ncol(mat)){

      # Projection of column k onto Q[,j]
      R[j, k] <- sum(Q[, j] * mat[, k])

      # Remove projection
      mat[, k] <- mat[, k] - Q[, j] * R[j, k]
    }
  }
}


beta_hat <- drop(solve(R) %*% t(Q) %*% iris$Petal.Length)
names(beta_hat) <- colnames(mat)

