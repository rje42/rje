test_that("combinations() behaves correctly in degenerate cases",
  {
    expect_identical(combinations(c()), matrix(0,1,0))
    expect_identical(combinations(c(0)), matrix(0,0,1))
    expect_identical(combinations(c(0,0)), matrix(0,0,2))
  })

test_that("powerSetMat() behaves correctly in degenerate cases",
{
  expect_identical(powerSetMat(0), matrix(0,1,0))
  expect_error(powerSetMat(-1))
})

