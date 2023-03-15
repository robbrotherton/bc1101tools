#' Title
#'
#' @param x
#' @param y
#' @param mu
#' @param related
#'
#' @return
#' @export
#'
#' @examples
example_t <- function(x, y = NULL, mu = NULL, related = FALSE) {

  list(mean_x = mean(x),
       mean_y = mean(y),

       df_x = length(x) - 1,
       df_y = length(y) - 1,
       DF = (length(x) - 1) + (length(y) - 1),

       var_x = var(x),
       var_y = var(y),

       pooled_var = s2p(x, y),
       std_err = std_err(x, y),

       t = (mean(x) - mean(y)) / std_err(x, y)
       )
}


#' Title
#'
#' @param x
#' @param y
#' @param z
#'
#' @return
#' @export
#'
#' @examples
example_ANOVA <- function(x, y, z = NULL) {

  warn<-options(warn=-1)

  n_x = length(x)
  n_y = length(y)
  n_z = length(z)

  if(!is.null(z)) {
    df <- data.frame(x, y, z)
  } else {
    df <- data.frame(x, y)
  }

  N <- length(c(x, y, z))
  n <- length(x)
  k <- 3
  G <- sum(c(x, y, z))
  P <- rowSums(df)

  SS_between_subjects <- sum(P^2/k) - G^2/N
  SS_within = SS(x) + SS(y) + SS(z)
  SS_between = SS(c(x, y, z)) - (SS(x) + SS(y) + SS(z))
  SS_error = SS_within - SS_between_subjects
  SS_total = SS(c(x, y, z))

  df_error = (N - k) - (n - 1)

  k = sum(c(!is.null((x)), !is.null((y)), !is.null((z))))

  MS_within = (SS(x) + SS(y) + SS(z)) / ((n_x - 1) + (n_y - 1) + pmax((n_z - 1), 0))
  MS_error <- SS_error / df_error
  MS_between = (SS(c(x, y, z)) - (SS(x) + SS(y) + SS(z))) / (k - 1)

  out <- list(
    n_x = length(x),
    n_y = length(y),
    n_z = length(z),

    mean_x = mean(x),
    mean_y = mean(y),
    mean_z = mean(z),

    SS_x = SS(x),
    SS_y = SS(y),
    SS_z = SS(z),
    SS_within = SS(x) + SS(y) + SS(z),
    SS_between = SS(c(x, y, z)) - (SS(x) + SS(y) + SS(z)),
    SS_between_subjects = SS_between_subjects,
    SS_error = SS_error,
    SS_total = SS(c(x, y, z)),

    df_total = length(c(x, y, z)) - 1,
    df_within = (n_x - 1) + (n_y - 1) + pmax((n_z - 1), 0),
    df_between = k - 1,
    df_between_subjects = n - 1,
    df_error = (N - k) - (n - 1),

    MS_total = SS(c(x, y, z)) / (length(c(x, y, z)) - 1),
    MS_within = MS_within,
    MS_error = MS_error,
    MS_between = MS_between,
    f_ratio = MS_between / MS_within,
    f_related = MS_between / MS_error,

    eta_sq = SS_between / SS_total,
    eta_sq_partial = SS_between / (SS_total - SS_between_subjects)
  )

  options(warn)
  if (is.null(z)) {
    out[!stringr::str_detect(names(out), "z")]
  } else {
    out
  }
}



#' Title
#'
#' @param x
#' @param y
#' @param z
#'
#' @return
#' @export
#'
#' @examples
example_ANOVA_factorial <- function(data) {

  s <- summary(aov(DV ~ A*B, data = data))
  df <- s[[1]]$Df
  ss <- s[[1]]$`Sum Sq`
  ms <- s[[1]]$`Mean Sq`
  f <- s[[1]]$`F value`

  list(
    N = nrow(data),
    n = nrow(data) / 4,
    k = 4,
    k_a = 2,
    k_b = 2,
    G = sum(data$DV),
    sigma_x_sq = sum(data$DV^2),

    T1 = dplyr::filter(data, A==1 & B==1) |> dplyr::pull(DV) |> sum(),
    T2 = dplyr::filter(data, A==2 & B==1) |> dplyr::pull(DV) |> sum(),
    T3 = dplyr::filter(data, A==1 & B==2) |> dplyr::pull(DV) |> sum(),
    T4 = dplyr::filter(data, A==2 & B==2) |> dplyr::pull(DV) |> sum(),

    SS1 = dplyr::filter(data, A==1 & B==1) |> dplyr::pull(DV) |> SS(),
    SS2 = dplyr::filter(data, A==2 & B==1) |> dplyr::pull(DV) |> SS(),
    SS3 = dplyr::filter(data, A==1 & B==2) |> dplyr::pull(DV) |> SS(),
    SS4 = dplyr::filter(data, A==2 & B==2) |> dplyr::pull(DV) |> SS(),

    df_a = df[1],
    df_b = df[2],
    df_ab = df[3],
    df_within = df[4],
    df_between = sum(df[1:3]),
    df_total = sum(df),

    ss_a = ss[1],
    ss_b = ss[2],
    ss_ab = ss[3],
    ss_within = ss[4],
    ss_between = sum(ss[1:3]),
    ss_total = sum(ss),

    ms_a = ms[1],
    ms_b = ms[2],
    ms_ab = ms[3],
    ms_within = ms[4],
    ms_between = sum(ms[1:3]),
    ms_total = sum(ms),

    f_a = f[1],
    f_b = f[2],
    f_ab = f[3]
  )

}



#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
example_correlation <- function(x, y) {

  p <- (x-mean(x)) * (y-mean(y))
  sp <- sum(p)
  ss_x <- sum((x-mean(x))^2)
  ss_y <- sum((y-mean(y))^2)
  r <- sp/sqrt(ss_x * ss_y)

  list(
    p = p,
    sp = sp,
    ss_x = ss_x,
    ss_y = ss_y,
    r = r
  )
}



# Helper functions --------------------------------------------------------

SS <- function(x) sum((x - mean(x))^2)

std_err <- function(x, y = NULL) {

  if (!is.null(y)) {
    pooled_var = s2p(x, y)
    sqrt(pooled_var / length(x) + pooled_var / length(y))
  }
}


s2p <- function(x, y) {
  df_x <- length(x) - 1
  df_y <- length(y) - 1

  numerator <- (var(x) * df_x + var(y) * df_x)
  denominator <- df_x + df_y

  numerator / denominator

}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
descriptives <- function(x) {
  M <- sprintf("%.2f", mean(x))
  SD <- sprintf("%.2f", sd(x))
  htmltools::HTML(glue::glue("(\\(M = {M}\\); \\(SD = {SD}\\))"))
}


