#' Standard deviation table
#'
#' @param x
#' @param population_sd
#' @param include
#' @param digits
#' @param align
#' @param x_var_name
#'
#' @return
#' @export
#'
#' @examples
SD_table <- function(x, population_sd = FALSE, include = c("mean", "devs", "devs_sq", "ss", "var", "sd"), digits = 2, align = "r", x_var_name = "\\(X\\)") {

  n <- length(x)
  devs <- x-mean(x)
  devs2 <- devs^2

  format_numbers <- function(x, digits) {
    sprintf(glue::glue("%.{digits}f"), round(x, digits))
  }

  M <- mean(x) |> format_numbers(digits)
  SS <- sum(devs2) |> format_numbers(digits)

  s2 <- ifelse(population_sd,
               sum(devs2)/(n),
               sum(devs2)/(n-1))

  SD <- sqrt(s2)

  s2 <- format_numbers(s2, digits)
  SD <- format_numbers(SD, digits)


  devs <- if("devs" %in% include) round(devs, digits) else rep("", length(x))
  devs2 <- if("devs_sq" %in% include) round(devs2, digits) else rep("", length(x))

  dat <- data.frame(X = x,
                    devs,
                    devs2) |>
    dplyr::mutate_all(as.character)

  symbol <- ifelse(population_sd, "\\sigma", "s")

  summary_mean <- ifelse("mean" %in% include, glue::glue("\\(M = {M}\\)"), "&nbsp;")
  summary_ss   <- ifelse("ss" %in% include, glue::glue("\\(SS = {SS}\\)"), "&nbsp;")
  summary_var  <- ifelse("var" %in% include, glue::glue("\\({symbol}^2 = {s2}\\)"), "&nbsp;")
  summary_sd   <- ifelse("sd" %in% include, glue::glue("\\({symbol} = {SD}\\)"), "&nbsp;")

  summary_rows <- rbind(
    c(X = summary_mean, devs = "", devs2 = summary_ss),
    c(X = "",           devs = "", devs2 = summary_var),
    c(X = "",           devs = "", devs2 = summary_sd)
  )

  dat <- rbind(dat, summary_rows)

  col.names <- c(x_var_name,"\\(X-M\\)","\\((X-M)^2\\)")

  tab <- knitr::kable(dat, format = "html", escape = FALSE, align = align,
                      col.names = col.names,
                      table.attr = "style='width:33.3%;'")


  tab <- tab |>
    # kableExtra::kable_styling(full_width = TRUE) |>
    kableExtra::column_spec(1:3, width = "30%") |>
    kableExtra::row_spec(1:(nrow(dat)), extra_css = "border: none;") |>
    kableExtra::row_spec((nrow(dat)-2):nrow(dat), extra_css = "font-size: 0.8em;") |>
    kableExtra::row_spec(nrow(dat)-3, extra_css = "border-bottom: 2px solid")

  tab
}


