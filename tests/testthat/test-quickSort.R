L <- list(c(1:2), c(2:3), c(1,4), 1:4)

f <- function(x,y,c=1) c*greaterThan(x,y)

set.seed(123)
x <- rnorm(10)

test_that("quickSort works", {
  expect_equal(subsetOrder(L[[1]], L[[2]]), 0L)
  expect_equal(quickSort(L, subsetOrder), c(1L, 1L, 1L, 2L))
  expect_equal(quickSort(x, f), rank(x))
  expect_equal(quickSort(x, f, c=-1), rank(-x))
  expect_equal(quickSort(x, function(x,y) -greaterThan(x,y)), rank(-x))
})
