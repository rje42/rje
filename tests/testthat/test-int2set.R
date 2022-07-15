x <- list(1:2, 3:5, 2:4)
n <- set2int(x)
x2 <- int2set(n)
n2 <- set2int(x2)

test_that("integer set representation works", {
  expect_equal(x, x2)
  expect_equal(n, n2)
})
