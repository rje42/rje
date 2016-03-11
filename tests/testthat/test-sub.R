p <- array(rnorm(27), rep(3,3))

test_that("subtable works", {
  expect_identical(subtable(p, 1:2, list(1,1)), p[1,1,])  # note that, in particular, this is a vector not an array of dimension 1
  expect_identical(subtable(p, 1:2, list(1,1), drop=FALSE), p[1,1,,drop=FALSE])
  expect_identical(subtable(p, 1:2, list(1:2,1)), p[1:2,1,])
  expect_identical(subtable(p, 1:2, list(1:2,1), drop=FALSE), p[1:2,1,,drop=FALSE])
})
