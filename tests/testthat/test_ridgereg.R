data(iris)



testthat::test_that("ridgereg works as MASS::lm.ridge", {
  formulas <- list(
    Petal.Length ~ Sepal.Length + Sepal.Width,
    Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width
  )

  lambdas <- c(
    0,
    0.0001,
    0.5,
    1,
    10,
    50
  )
  n <- nrow(iris)

  for (lambda in lambdas) {
    for (formula in formulas) {
      model <- ridgereg(
        formula = formula,
        data = iris,
        lambda = lambda * (n - 1) / n # Adjust for MASS::lm.ridge scaling convention. # nolint
      )
      reference <- MASS::lm.ridge(
        formula = formula,
        data = iris,
        lambda = lambda
      )

      expect_equal(
        unname(coef(model)),
        unname(coef(reference))
      )
    }
  }
})

## Kanskse behöver lägga till mer tests, jag förstod det på 
# beskrivningen som att detta är det enda som krävs men test 
# för fel input, och methoderna kankse också ska med?