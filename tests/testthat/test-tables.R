test_that("frequency table function creates html table", {

  # simple 2-column frequency table with calculated frequencies
  expect_equal(frequency_table(1:5),
               knitr::kable(data.frame(1:5, 1) |> setNames(c("\\(X\\)", "\\(f\\)")),
                            format = "html",
                            align = "c"))


  # more complicated example specifying x values and frequencies
  expected_df <- tibble::tibble(x = 1:5,
                                f = c(3, 5, 4, 2, 1),
                                p = c(0.20, 0.33333, 0.2666667, 0.1333333, 0.066667),
                                per = p * 100,
                                cum_per = cumsum(per)) |>
    setNames(c("\\(X\\)", "\\(f\\)", "Proportion", "Percent", "Cumulative Percent"))

  expected_output <- knitr::kable(expected_df,
                                  format = "html",
                                  align = "c",
                                  digits = 2)

  expect_equal(frequency_table(x = 1:5, f = c(3, 5, 4, 2, 1), additional_cols = TRUE),
               expected_output)
})
