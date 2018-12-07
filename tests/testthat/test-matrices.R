set.seed(4901)
x <- rnorm(8)

test_that("fastHadamard() is equivalent to multiplication by design matrix",
  {
    expect_equal(c(designMatrix(3) %*% x), fastHadamard(x))
  })
