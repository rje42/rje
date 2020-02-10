L <- list(c(1:2), c(2:3), c(1,4), 1:4)

set.seed(123)

test_that("quickSort works", {
  expect_equal(subsetOrder(L[[1]], L[[2]]), 0L)
  expect_equal(quickSort(L, subsetOrder), c(1L, 1L, 1L, 2L))
})
