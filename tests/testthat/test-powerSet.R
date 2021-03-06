x <- 1:2
y <- 3:4

test_that("powerSet works as expected", {
  expect_equal(powerSet(integer(0)), 
               list(integer(0)))
  expect_equal(powerSet(x), 
               list(integer(0), 1, 2, 1:2))
  expect_equal(powerSet(1:3, m=2), 
               list(integer(0), 1, 2, 1:2, 3, c(1,3), 2:3))
})

test_that("powerSetCond works as expected", {
  expect_equal(powerSetCond(x, y, sort=TRUE), 
               list(1, 2, 1:2, c(1,3), c(2,3), 1:3, c(1,4), 
                    c(2,4), c(1,2,4), c(1,3,4), c(2,3,4), 1:4))
  expect_equal(powerSetCond(x, y, sort=TRUE, m=3), 
               list(1, 2, 1:2, c(1,3), c(2,3), 1:3, c(1,4), 
                    c(2,4), c(1,2,4), c(1,3,4), c(2,3,4)))
})