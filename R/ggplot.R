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
