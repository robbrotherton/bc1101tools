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
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0,.01)))

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

    arrow_geom <- ggplot2::geom_segment(ggplot2::aes(x = ggplot2::arrow, xend = ggplot2::arrow,
                                                     y = ggplot2::arrow_y, yend = 0),
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



plot_CI <- function(confidence = .95, dist = "norm", n = 100, df = 99, labels = "all", M = 0, SD = 1, std.err = NULL) {

  if (labels=="all") {
    labels <- c("point","width","t","M","info")
  }

  r <- function(x) sprintf("%.2f", x)

  alpha <- 1 - confidence
  sM <- ifelse(is.null(std.err), SD/sqrt(n), std.err)

  if (dist=="norm") {
    fun <- dnorm
    args = NULL
    xlim = qnorm(c(alpha/2, 1-alpha/2))
    y <- fun(0)
    y_width <- fun(xlim)
  }

  if (dist=="t") {
    fun <- dt
    args <- list(df = df)
    xlim = qt(c(alpha/2, 1-alpha/2), df = df)
    y <- fun(0, df = df)
    y_width <- fun(xlim, df = df)
  }

  plot <- ggplot2::ggplot() +
    # ggplot2::geom_line(stat = "function", fun = fun,args = args, xlim = c(-4, 4), color = "black") +
    ggplot2::stat_function(geom = "line", fun = fun, args = args, color = "black", xlim = c(-4, 4)) +
    ggplot2::stat_function(geom = "area", fun = fun, args = args, fill = "lightblue", alpha = .5, xlim = xlim) +
    ggplot2::scale_y_continuous(limits = c(-.015, .4)) +
    ggplot2::theme_void()

  if ("point" %in% labels) {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = 0, y = 0)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 0, y = y, yend = 0))
  }

  if ("width" %in% labels) {
    plot <- plot +
      ggplot2::geom_segment(ggplot2::aes(x = xlim[1], xend = xlim[2], y = y_width[1]/2, yend = y_width[2]/2), linetype = 2,
                   arrow = ggplot2::arrow(ends = "both", length = ggplot2::unit(0.15, "inches")))
  }

  if ("t" %in% labels) {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(x = c(xlim, 0), y = 0, label = c(round(xlim, 2), "t = 0")), vjust = 1.2)
  }

  if ("M" %in% labels) {
    M1 <- r(M + xlim[1] * sM)
    M2 <- r(M + xlim[2] * sM)
    M_ <- r(M)

    plot <- plot +
      ggrepel::geom_text_repel(ggplot2::aes(x = c(xlim, 0), y = c(y_width, y), label = c(M1,M2, M_)), nudge_y = .04)
  }

  if ("info" %in% labels) {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(x = -4, y = .4), hjust = 0, vjust = 1, family = "serif", size = 8,
                label = paste0("If...\n",
                               "Confidence = ", confidence * 100, "%\n",
                               "n = ", n, "\n",
                               "M = ", round(M, 2), "\n",
                               "SD = ", round(SD, 2), "\n",
                               expression(s[M]==3), round(sM, 2)))
  }

  plot

}



#' Plot 2x2 generic
#'
#' @param A1B1
#' @param A1B2
#' @param A2B1
#' @param A2B2
#'
#' @return
#' @export
#'
#' @examples
plot_2x2_generic <- function(A1B1, A1B2, A2B1, A2B2) {

  data <- data.frame(IV1 = c("A1","A1","A2","A2"),
                     IV2 = c("B1","B2","B1","B2"),
                     value = c(A1B1, A1B2, A2B1, A2B2))

  ggplot2::ggplot(data, ggplot2::aes(x = IV1, y = value, linetype = IV2, group = IV2)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(limits = c(0.5,2.5), breaks = c(1,2), labels = c("Low","High")) +
    ggplot2::labs(y = "DV", x = "Factor A", linetype = "Factor B") +
    theme_bc1101() +
    ggplot2::theme(legend.background = ggplot2::element_blank())
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


#' Title
#'
#' @param filename
#' @param plot
#' @param dir
#' @param subdir
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
plot_save <- function(filename,
                      plot = ggplot2::last_plot(),
                      dir = "slides",
                      subdir = "media",
                      width = 5, height = 3) {

  ggplot2::ggsave(filename = here::here(dir, subdir, filename),
                  plot = plot,
                  width = width, height = height)

}
