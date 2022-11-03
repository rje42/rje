set.seed(123)
k <- 4
x <- rnorm(2^k)

test_that("Moebius transformation works", {
  expect_equal(c(subsetMatrix(k) %*% x), invMobius(x))
  expect_equal(c(abs(subsetMatrix(k)) %*% x), fastMobius(x))
})

test_that("Hadamard transformation works", {
  expect_equal(c(designMatrix(k) %*% x), fastHadamard(x))
})
