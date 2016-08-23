set.seed(123)

p <- rprobdist(c(5,3,4,2))

test_that("propTable() works the same as prop.table()", {
  expect_equal(prop.table(p, c(1,4)), propTable(p, c(1,4)))
  expect_equal(propTable(p, c(4,2)), propTable(p, c(4,2)))
})

test_that("conditionTable() gives degenerate objects of correct dimension", {
  expect_identical(dim(conditionTable(p,3,1:2)), c(4L,5L,3L))
})

test_that("conditionTable() gives correct answer under different orderings", {
  expect_equal(conditionTable(p, 2, c(1,3), order=FALSE), aperm(conditionTable(p, 2, c(1,3), order=TRUE), c(2,1,3)))
  expect_equal(conditionTable(p, c(2,4), c(1,3), order=FALSE), aperm(conditionTable(p, c(2,4), c(1,3), order=TRUE), c(3,1,4,2)))
})
