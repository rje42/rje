set.seed(123)

test_that("rprobdist gives degenerate objects of correct dimension", {
  expect_identical(rprobdist(0), numeric(0))
  expect_identical(rprobdist(c(0,0)), matrix(0,0,0))
  expect_identical(rprobdist(0, 2), matrix(0,0,0))
  expect_identical(rprobdist(c(0,0,0)), array(0,c(0,0,0)))
  expect_identical(rprobdist(0, 3), array(0,c(0,0,0)))
  expect_identical(dim(rprobdist(c(2,0,2))), c(2L,0L,2L))

})

test_that("rprobdist gives probability distributions", {
  expect_true(all(rprobdist(3, 4) >= 0))
  expect_equal(sum(rprobdist(3, 4)), 1)
})

test_that("rprobdist returns errors appropriately", {
  expect_error(rprobdist(c(2,2,2), d=2))
  expect_error(rprobdist(c(2,-1,2)))
  expect_error(rprobdist(c(2,2,2), alpha=-1))
})

test_that("rprobdist warns for recycling", {
  expect_warning(rprobdist(c(2,2), d=5))
})
