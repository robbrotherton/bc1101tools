test_that("example t data generator works", {
  res <- example_t(c(9,11,13),c(3,5,4))
  expect_equal(res$t, 5.42, tolerance = 0.001)
})


test_that("example ANOVA data generator works", {

  # 2-group anova, should be equivalent to t-test above
  res <- example_ANOVA(c(9,11,13),c(3,5,4))
  expect_equal(res$f_ratio, 5.42^2, tolerance = 0.001)

  # 3-group ANOVA
  res <- example_ANOVA(c(9,11,13),c(3,5,4),c(5,6,7))
  expect_equal(res$f_ratio, 19.5)
})
