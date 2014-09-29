set.seed(123)
x1 = sample(c(TRUE,FALSE), 1e3, replace=TRUE)
x2 = sample(c(TRUE,FALSE), 1e3, replace=TRUE)

test_that("and0 same as & for logical vectors", {
  expect_identical(and0(x1,x2), x1 & x2)
})

test_that("or0 same as | for logical vectors", {
  expect_identical(or0(x1,x2), x1 | x2)
})

# random numbers, some of which are 0
y1 = rnorm(1e3)
y2 = rnorm(1e3)
y1[x1] = 0; y2[x2] = 0

test_that("and0 same as & for numerical vectors", {
  expect_identical(and0(y1,y2), y1 & y2)
})

test_that("or0 same as | for numerical vectors", {
  expect_identical(or0(y1,y2), y1 | y2)
})
