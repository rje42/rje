y <- matrix(c(1,3,4,
              2,3,5,
              1,2,4,
              2,3,4), 4, 3, byrow=TRUE)
x <- rbind(y[c(1,3,4,4,1),], c(4,5,6))

test_that("match_rows works", {
  expect_equal(match_rows(x,y), c(1,3,4,4,1,NA))
})
