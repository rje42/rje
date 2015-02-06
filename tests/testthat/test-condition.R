set.seed(123)

p <- rprobdist(c(5,3,4,2))

test_that("conditionTable() gives degenerate objects of correct dimension", {
  expect_identical(dim(conditionTable(p,3,1:2)), c(4L,5L,3L))
})
