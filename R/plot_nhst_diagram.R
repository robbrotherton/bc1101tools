
#' Title
#'
#' @param text.size
#' @param pop1lab
#' @param pop1color
#' @param pop1line
#' @param pop2lab
#' @param pop2color
#' @param pop2fill
#' @param pop2line
#' @param sample1lab
#' @param sample1color
#' @param sample1line
#' @param sample2lab
#' @param sample2color
#' @param sample2fill
#' @param sample2line
#' @param sample_line
#'
#' @return
#' @export
#'
#' @examples
plot_nhst_diagram <- function(text.size = 7,
                         pop1lab = "Known\nOriginal\npopulation", pop1color = "black", pop1line = 1,
                         pop2lab = "Unknown\nTreated\npopulation", pop2color = "black", pop2fill = "lightskyblue2", pop2line = 2,
                         sample1lab = "Sample", sample1color = "black", sample1line = 1,
                         sample2lab = "Treated\nSample", sample2color = "black", sample2fill = "lightskyblue2", sample2line = 1,
                         sample_line = "black") {



  ggplot2::ggplot(NULL) +

    # top-left circle (original population)
    ggforce::geom_circle(ggplot2::aes(x0 = 6, y0 = 6, r = .8), linetype = pop1line) +
    ggplot2::geom_text(ggplot2::aes(x = 6, y = 6), label = pop1lab, size = text.size, color = pop1color) +

    # top-right circle  (treated population)
    ggforce::geom_circle(ggplot2::aes(x0 = 9, y0 = 6, r = .8), linetype = pop2line, fill = pop2fill) +
    ggplot2::geom_text(ggplot2::aes(x = 9, y = 6), label = pop2lab, size = text.size, color = pop2color) +

    # bottom-left box (untreated sample)
    ggplot2::geom_rect(ggplot2::aes(xmin = 5.25, xmax = 6.75, ymin = 3.5, ymax = 4.5), fill = NA, color = sample1color, linetype = sample1line) +
    ggplot2::geom_text(ggplot2::aes(x = 6, y = 4), label = sample1lab, size = text.size) +

    # bottom-right box (treated sample)
    ggplot2::geom_rect(ggplot2::aes(xmin = 8.25, xmax = 9.75, ymin = 3.5, ymax = 4.5), fill = sample2fill, color = sample2color, linetype = sample2line) +
    ggplot2::geom_text(ggplot2::aes(x = 9, y = 4), label = sample2lab, size = text.size) +

    # lines
    ggplot2::geom_segment(ggplot2::aes(x = 6, xend = 6, y = 5.2, yend=4.5)) + # down
    ggplot2::geom_segment(ggplot2::aes(x = 9, xend = 9, y = 5.2, yend=4.5), linetype = 2) + # up
    ggplot2::geom_segment(ggplot2::aes(x = 6.75, xend = 8.25, y = 4, yend=4), color = sample_line) + # bottom

    # treatment
    ggplot2::geom_rect(ggplot2::aes(xmin = 7.25, xmax = 7.75, ymin = 3.5, ymax = 6.8), fill = "lightskyblue2", color = NA) +
    ggplot2::geom_text(ggplot2::aes(x = 7.5, y = 5.1), label = "T\nr\ne\na\nt\nm\ne\nn\nt", size = text.size) +

    # curve between populations
    ggplot2::geom_curve(ggplot2::aes(x = 6.65, xend = 8.35, y = 6.5, yend=6.5), curvature = -0.2, linetype = 8,
               arrow = ggplot2::arrow(length = ggplot2::unit(.1, "inches"), ends = "both", type = "closed"),
               color = "black") + # top
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

}
