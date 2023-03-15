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


#' Standard deviation table
#'
#' @param x
#' @param y
#' @param digits
#' @param digits
#' @param align
#'
#' @return
#' @export
#'
#' @examples
SP_table <- function(x, y, digits = 2, align = "r") {

  n <- length(x)
  x_devs <- x-mean(x)
  x_devs2 <- x_devs^2

  y_devs <- y-mean(y)
  y_devs2 <- x_devs^2

  f <- function(x, digits = 2) {
    sprintf(glue::glue("%.{digits}f"), round(x, digits))
  }

  m_x <- mean(x)
  m_y <- mean(y)
  ss_x <- sum(x_devs2)
  ss_y <- sum(y_devs2)

  var_x <- sum(x_devs2)/(n-1)
  var_y <- sum(y_devs2)/(n-1)

  sd_x <- sqrt(var_x)
  sd_y <- sqrt(var_y)

  sp <- x_devs * y_devs

  dat <- data.frame(x = x,
                    x_devs,
                    x_devs2,
                    y = y,
                    y_devs,
                    y_devs2,
                    p = sp) |>
    dplyr::mutate_all(as.character)

  symbol <- "s"

  summary_m_x <- glue::glue("\\(M_X = {f(m_x)}\\)")
  summary_m_y <- glue::glue("\\(M_Y = {f(m_y)}\\)")
  summary_ss_x <- glue::glue("\\(SS_X = {f(ss_x)}\\)")
  summary_ss_y <- glue::glue("\\(SS_Y = {f(ss_y)}\\)")
  summary_var_x <- glue::glue("\\(s^2_X = {f(var_x)}\\)")
  summary_var_y <- glue::glue("\\(s^2_Y = {f(var_y)}\\)")
  summary_sd_x <- glue::glue("\\(s_X = {f(sd_x)}\\)")
  summary_sd_y <- glue::glue("\\(s_Y = {f(sd_y)}\\)")
  summary_sp <- glue::glue("\\(SP = {f(sum(sp))}\\)")

  summary_rows <- rbind(
    c(x = summary_m_x, x_devs = "", x_devs2 = summary_ss_x,  y = summary_m_y, y_devs = "", y_devs2 = summary_ss_y, p = summary_sp),
    c(x = "",  x_devs = "", x_devs2 = summary_var_x, y = "",  y_devs = "", y_devs2 = summary_var_y, p = ""),
    c(x = "",  x_devs = "", x_devs2 = summary_sd_x,  y = "",  y_devs = "", y_devs2 = summary_sd_y, p = "")
  )

  dat <- rbind(dat, summary_rows)

  col.names <- c("\\(X\\)","\\(X-M_X\\)","\\((X-M_X)^2\\)", "\\(Y\\)","\\(Y-M_Y\\)","\\((Y-M_Y)^2\\)", "\\(P\\)")

  tab <- knitr::kable(dat, format = "html", escape = FALSE, align = align,
                      col.names = col.names)


  tab <- tab |>
    # kableExtra::kable_styling(full_width = TRUE) |>
    # kableExtra::column_spec(1:3, width = "30%") |>
    # kableExtra::row_spec(1:(nrow(dat)), extra_css = "border: none;") |>
    kableExtra::row_spec((nrow(dat)-2):nrow(dat), extra_css = "font-size: 0.8em;") |>
    kableExtra::row_spec(nrow(dat)-3, extra_css = "border-bottom: 2px solid")

  tab
}


