#
# labs <- list(lines = lines <- c(-2,-1,-0,1,2),
#              x = c(-2.8, -1.5, -0.5, 0.5, 1.5, 2.8),
#              y = c(0.03, 0.06, 0.16, 0.16, 0.06, 0.03),
#              lab = c("2.28%", "13.59%", "34.13%", "34.13%", "13.59%", "2.28%"))
#
# norm_plain <- ggplot(NULL, aes(x = c(-3.5,3.5))) +
#   stat_function(fun = "dnorm") +
#   # stat_function(fun = "dnorm", geom = "area", fill = "thistle", alpha = .3, color = "black") +
#   # stat_function(fun = "dnorm", geom = "area", fill = "plum", alpha = .3, xlim = c(-2,2)) +
#   # stat_function(fun = "dnorm", geom = "area", fill = "darkorchid", alpha = .3,  xlim = c(-1,1)) +
#   scale_x_continuous(breaks = -3:3) +
#   scale_y_continuous(expand = c(0,0)) +
#   geom_segment(aes(x = labs$lines, xend = labs$lines, y = 0, yend = dnorm(labs$lines)), linetype = 2) +
#   # annotate("text", x = labs$x, y = labs$y, label = labs$lab) +
#   labs(x = "z-score", y = NULL) +
#   theme(axis.line.x = element_line(color = "black"),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.background = element_blank())
#
# norm <- ggplot(NULL, aes(x = c(-3.5,3.5))) +
#   stat_function(fun = "dnorm", geom = "area", fill = "thistle", alpha = .3, color = "black") +
#   stat_function(fun = "dnorm", geom = "area", fill = "plum", alpha = .3, xlim = c(-2,2)) +
#   stat_function(fun = "dnorm", geom = "area", fill = "darkorchid", alpha = .3,  xlim = c(-1,1)) +
#   scale_x_continuous(breaks = -3:3) +
#   scale_y_continuous(expand = c(0,0)) +
#   geom_segment(aes(x = labs$lines, xend = labs$lines, y = 0, yend = dnorm(labs$lines)), linetype = 2) +
#   # annotate("text", x = labs$x, y = labs$y, label = labs$lab) +
#   labs(x = "z-score", y = NULL) +
#   theme(axis.line.x = element_line(color = "black"),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.background = element_blank())
#
# norm_labeled <- norm + annotate("text", x = labs$x, y = labs$y, label = labs$lab)
