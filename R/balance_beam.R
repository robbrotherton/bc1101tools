#' Balance beam plot
#'
#' @param data A vector of integers.
#' @param label_boxes Logical. Show labels in boxes with their deviation scores.
#' @param size The size of text labels.
#'
#' @return A ggplot2 plot showing a box histogram of scores perched on a
#'   triangle positioned at the mean of the data
#' @export
#'
#' @examples balance_beam(c(1, 2, 6, 6, 10))
balance_beam <- function(data, label_boxes = FALSE, size = 4) {

  df <- tibble::tibble(x = data, group = factor(seq_along(x)))

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = df, ggplot2::aes(x = x, group = group), binwidth = 1, color = "black", fill = "grey") +
    ggplot2::scale_x_continuous(breaks = unique(data)) +
    ggplot2::coord_fixed() +
    geom_balance(data, size = size) +
    ggplot2::theme_void() +
    NULL

  if (label_boxes) {
    plot <- plot + geom_labels(data, size = size)
  }

  return(plot)

}


geom_balance <- function(data, size = 4){

  x_lims <- c(min(data) - 0.5, max(data) + 0.5)
  labs <- seq(min(data), max(data), by = 1)

  list(
    # balance beam
    ggplot2::geom_polygon(ggplot2::aes(x = rep(x_lims, each = 2), y = c(-.2, 0, 0, -.2))),
    ggplot2::geom_text(ggplot2::aes(x = labs, y = -.08, label = labs),
              size = size * 0.5,
              color = "white"),
    # triangle
    ggforce::geom_regon(ggplot2::aes(x0 = mean(data), y0 = -.7-.2, r = .7, sides = 3, angle = 0), fill = "red", color = "black"),
    ggplot2::geom_text(ggplot2::aes(x = mean(data), y = -.6-.2), label  = "M",
              fontface = "italic", family = "serif",
              size = size)
  )
}


geom_labels <- function(data, size){

  dat <- data.frame(x = data) |>
    dplyr::group_by(x) |>
    dplyr::mutate(y = 1:dplyr::n() - .5,
           lab = x - mean(data),
           color = dplyr::case_when(
             lab < 0 ~ "dark red",
             lab > 0 ~ "dark green",
             TRUE ~ "black"
           ))

  list(ggplot2::geom_text(data = NULL, ggplot2::aes(x = dat$x, y = dat$y, label = dat$lab,
                                               color = dat$color),
                          size = size),
    ggplot2::scale_color_identity())

}



