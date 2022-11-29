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

test_that("factorial ANOVA data generator works", {

  banana_math <- c(7,9,8,9)
  banana_rt <- c(5,4,6,5)
  candy_math <- c(5,3,4,4)
  candy_rt <- c(6,6,5,5)

  df <- data.frame(A = rep(c(1,2), each = 8),
                   B = rep(c(1,2), each = 4),
                   DV = c(banana_math, banana_rt, candy_math, candy_rt))

  res <- bc1101tools::example_ANOVA_factorial(df)

  expect_equal(res$f_ab, 34.94, tolerance = 0.001)

})
