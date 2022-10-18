#' Title
#'
#' @param x
#' @param xlab
#' @param ylab
#' @param block
#'
#' @return
#' @export
#'
#' @examples
plot_histogram <- function(x, binwidth = 1, xlab = "Score", ylab = "Frequency", block = FALSE) {

  id <- if(block) seq_along(x) else NULL

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = x, group = id),
                            binwidth = binwidth, color = 'black', fill = "#7f7f7f") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, .02))) +
    ggplot2::labs(x = xlab, y = ylab)

  if (block) plot + ggplot2::coord_equal() else plot

}


#' Title
#'
#' @param fun
#' @param args
#' @param xlim
#' @param segments
#' @param linetype
#' @param fill_colors
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
plot_distribution <- function(fun = dnorm,
                              args = NULL,
                              xlim = NULL,
                              xbreaks = NULL,
                              xlab = NULL,
                              segments = NULL,
                              linetype = 1,
                              fill_colors = NULL,
                              alpha = 0.7,
                              arrow = NULL,
                              arrow_y = 0.4,
                              arrow_size = 0.2) {

  if (!"mean" %in% names(args)) args$mean <- 0
  if (!"sd" %in% names(args)) args$sd <- 1

  if (is.null(xbreaks)) {
    xbreaks <- seq(from = args$mean - (3 * args$sd),
                   to = args$mean + (3 * args$sd),
                   by = args$sd)
  }

  if (is.null(xlim)) {
    xlim <- c(min(xbreaks), max(xbreaks))
  }

  base <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(breaks = xbreaks, name = xlab) +
    ggplot2::scale_y_continuous(expand = c(0,0))

  curve_geom <- ggplot2::stat_function(fun = fun, args = args, xlim = xlim)
  segment_geom <- NULL
  shaded_geoms <- NULL
  arrow_geom <- NULL

  if (!"mean" %in% names(args)) args$mean <- 0
  if (!"sd" %in% names(args)) args$sd <- 1

  density <- function(x) {
    if ("df" %in% names(args)) {
      dt(x, df = args[["df"]])
    } else if ("df1" %in% names(args)) {
      df(x, df1 = args[["df1"]], df2 = args[["df2"]])
    } else {
      dnorm(x, mean = args[["mean"]], sd = args[["sd"]])
    }
  }

  if (!is.null(segments)) {
    segment_data <- tibble::tibble(x = segments,
                                   yend = density(x))

    segment_geom <- ggplot2::geom_segment(data = segment_data,
                                          ggplot2::aes(x, y = 0, xend = x, yend = yend),
                                          linetype = linetype)
  }

  if (!is.null(fill_colors)) {
    shaded_region_data <- segment_data |>
      tibble::add_row(x = xlim[1], .before = 1) |>
      dplyr::mutate(xend = dplyr::lead(x, default = xlim[2]),
                    fill = fill_colors) |>
      dplyr::select(x, xend, fill)


    shaded_geoms <- purrr:::pmap(shaded_region_data,
                                 ~ggplot2::stat_function(fun = fun, args = args, geom = "area",
                                                         xlim = c(..1, ..2),
                                                         fill = ..3,
                                                         alpha = alpha))
  }

  if (!is.null(arrow)) {
    # y <- density(arrow) * arrow_length

    arrow_geom <- ggplot2::geom_segment(ggplot2::aes(x = arrow, xend = arrow,
                                                     y = arrow_y, yend = 0),
                                        arrow = ggplot2::arrow(type = "closed",
                                                               length = ggplot2::unit(arrow_size, "in")),
                                        color = "red")
  }


    base +

      shaded_geoms +

      segment_geom +

      curve_geom +

      arrow_geom

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_xonly <- function(yline = FALSE) {
  list(ggplot2::theme_void(),
       ggplot2::theme(axis.line.x = ggplot2::element_line(),
                      axis.ticks.x = ggplot2::element_line(),
                      axis.ticks.length = ggplot2::unit(.07, "in"),
                      axis.line.y = if(yline) ggplot2::element_line() else ggplot2::element_blank(),
                      axis.text.x = ggplot2::element_text(),
                      axis.title.x = ggplot2::element_text(),
                      plot.margin = ggplot2::margin(t = 3, b = 3))
  )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_bc1101 <- function() {
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 plot.background = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line())
}
