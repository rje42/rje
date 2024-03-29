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

fail <- list(1:5,1:2,2:4)
win <- list(1:5,2,2:4)

test_that("nested_sets works", {
  expect_equal(sets_nested(fail), NA)
  expect_equal(sets_nested(win), c(2L, 3L, 1L))
})
