#' Remove legend from ggplot
#'
#' @export
no_legend <- function() theme(legend.position="none")

#' Remove x-axis from ggplot
#'
#' @export
no_x_axis <- function () 
  theme(axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#' Remove y-axis from ggplot
#'
#' @export
no_y_axis <- function () 
  theme(axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#' ggplot default color vector
#'
#' Emulate ggplot default colors for a given number of categories. Credit to
#' John Colby. https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#'
#' @param n number of colors
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
