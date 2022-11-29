#' Make a frequency table
#'
#' @param x
#' @param f
#' @param values_name
#' @param binwidth
#' @param additional_cols
#'
#' @return
#' @export
#'
#' @examples
frequency_table <- function(x, f = NULL, values_name = "\\(X\\)", binwidth = 1, additional_cols = FALSE, digits = 2) {

  if (!is.null(f)) {
    data <- data.frame(x, f)
  } else {
    # round to the nearest multiple of binwidth...
    min <- round(min(x)/binwidth)*binwidth
    max <- round(max(x)/binwidth)*binwidth
    # need to add an extra bin if the max value==highest bin
    max <- ifelse(max(x)==max, max+1, max)

    breaks <-  seq(min, max, binwidth)
    g <- cut(x,
             breaks = breaks,
             right = FALSE)

    data <- as.data.frame(table(g))

    if (binwidth==1) {
      data[,1] <- breaks[1:nrow(data)]
    } else {
      data[,1] <- paste(breaks[1:nrow(data)], breaks[1:nrow(data)]+(binwidth-1), sep = "-")
    }
  }

  # Rename the columns to use mathematical symbols in html. Requires this escape notation.
  # See https://github.com/yihui/xaringan/issues/94
  data <- setNames(data, c(values_name, "\\(f\\)"))

  if (additional_cols) {
    data <- data |>
      dplyr::mutate(Proportion = dplyr::select(dplyr::cur_data_all(), 2) / sum(dplyr::select(dplyr::cur_data_all(), 2)),
                    Percent = Proportion * 100,
                    `Cumulative Percent` = cumsum(Percent)) |>
      dplyr::mutate(across(c(3, 4, 5), ~format(round(.x, digits = digits), nsmall = 2)))
  }

  knitr::kable(data, format = 'html', escape = F, align = 'c')

}



#' Title
#'
#' @param factor_a_name
#' @param factor_b_name
#' @param factor_a_levels
#' @param factor_b_levels
#' @param cell_values
#'
#' @return
#' @export
#'
#' @examples
anova_2x2_table <- function(factor_a_name = "Factor A",
                            factor_b_name = "Factor B",
                            factor_a_levels = c("\\(A_1\\)", "\\(A_2\\)"),
                            factor_b_levels = c("\\(B_1\\)", "\\(B_2\\)"),
                            cell_values = c("\\(A_1 B_1\\)","\\(A_2 B_1\\)","\\(A_2 B_1\\)","\\(A_2 B_2\\)")
                            ) {

  htmltools::HTML(glue::glue('
    <table>
        <thead>
            <tr>
                <th colspan="2" rowspan="2"></th>
                <th colspan="2" style="text-align: center;">{factor_a_name}</th>
            </tr>
            <tr>

                <th style="text-align: center;">{factor_a_levels[1]}</th><th style="text-align: center;">{factor_a_levels[2]}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td rowspan="2" style="vertical-align : middle;text-align:center;">{factor_b_name}</td>
                <td>{factor_b_levels[1]}</td>
                <td>{cell_values[1]}</td><td>{cell_values[2]}</td>
            </tr>
            <tr>
                <td>{factor_b_levels[2]}</td>
                <td>{cell_values[3]}</td><td>{cell_values[4]}</td>
            </tr>
        </tbody>
    </table>

                '))
}
