#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
pop_var <- function(x) {
  sum( (x - mean(x))^2 ) / length(x)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
pop_sd <- function(x) {
  sqrt(pop_var(x))
}
