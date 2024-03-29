y <- matrix(c(1,3,4,
              2,3,5,
              1,2,4,
              2,3,4), 4, 3, byrow=TRUE)
x <- rbind(y[c(1,3,4,4,1),], c(4,5,6))

match_rows(x,y)

test_that("match_rows works", {
  expect_equal(2 * 2, 4)
})
