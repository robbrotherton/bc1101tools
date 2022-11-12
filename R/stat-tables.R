#' t table
#'
#' @param dfs
#' @param qs
#' @param two_tailed_only
#' @param dots
#'
#' @return
#' @export
#'
#' @examples
t_table <- function(dfs = 1:15, alphas = c(.1, .05, .025, .01, .005), two_tailed_only = FALSE, dots = length(dfs)) {

  rounded <- function(x) sprintf("%.3f", x)


  data <- purrr::map_df(.x = alphas |> setNames(alphas),
                        .y = dfs,
                        .f = ~abs(qt(.x, .y))) |>
    tibble::add_column(df = dfs, .before = 1) |>
    dplyr::mutate(df = as.character(df),
                  dplyr::across(-"df", ~as.character(rounded(.x)))) |>
    tibble::add_row(df = "...", .after = dots) |>
    dplyr::mutate_all(~tidyr::replace_na(.x, "..."))

  replace_df_value <- nrow(data)/2
  data[nrow(data)/2, 1] <- glue::glue("\\(df\\) {paste(rep('&nbsp;', 5), collapse='')} {replace_df_value}")

  knitr::kable(data, col.names = c("Proportion<br>in 2 tails", alphas*2), escape = F, align = "r") |>
    kableExtra::add_header_above(c("Proportion\nin 1 tail", paste0("\n",alphas)), align = "r") |>
    kableExtra::column_spec(1, bold = TRUE)

}


# t_table()
# d <- tibble::add_column(d, data.frame(dfr = rep("df", nrow(d))), .before = 1)
# d
#
# knitr::kable(d, col.names = c("Proportion<br>in 2 tails", alphas), escape = F, align = "r") |>
#   kableExtra::add_header_above(c("Proportion\nin 1 tail", paste0("\n",alphas/2)), align = "r") |>
#   kableExtra::column_spec(1, bold = TRUE)


# t_table(dfs = 1:40,qs = .025, dots = c(11,30))
#
# t_tab_2_cols <- knitr::kable(t_tab[,c(1,4)], format = "html", escape=FALSE,
#                                         digits = 3,
#                                         align = "r",
#                                         col.names = c("<i>df</i>", ".05")) %>%
#   kableExtra::column_spec(1, bold = T)

# out_tab


#
# out_tab <- knitr::kable(t_tab, format = "html", escape=FALSE,
#                         digits = 3,
#                         align = "r",
#                         col.names = c("Proportion<br>in 2 tails", paste0("<br>",qs*2))) %>%
#   kableExtra::add_header_above(c("Proportion\nin 1 tail", paste0("\n",qs)), align = "r") %>%
#   kableExtra::column_spec(1, bold = T)
