#' Plots post-OBSP CI for mixed effects
#'
#' @param x An object of class \code{postOBSP_CI}
#' @param col Colors of CI
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param order_estimates Order of intervals in the plot
#' @param legend_position Legend position. Default: \code{NULL}
#' @param y_axis_lim Limits of the y axis. Default: \code{NULL}
#' @param ... Additional parameters
#'
#' @return
#' \item{plot_postOBSP}{Plot with confidence intervals for mixed effects}
#'
#' @importFrom ggplot2 ggplot aes coord_cartesian
#' geom_errorbar labs theme_bw scale_color_manual
#' theme element_text element_blank
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics plot
#'
#' @export
#'

plot.postOBSP_CI <- function(x,
                             col = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             order_estimates = NULL,
                             legend_position = NULL,
                             y_axis_lim = NULL,
                             ...) {
  if (is.null(xlab)) {
    xlab = "ID Cluster"
  }
  
  if (is.null(ylab)) {
    ylab = "Mixed Effect"
  }
  
  if (is.null(legend_position)) {
    legend_position = c(0.88, 0.88)
  }  
  
  if (is.null(col)) {
    col = "blue"
  }
  
  x_plot = 1:length(x$postOBSP_up)
  if (is.null(order_estimates)) {
    order_estimates = x_plot
  }
  mu_postOBSP_do = (x$postOBSP_do)[order_estimates]
  mu_postOBSP_up = (x$postOBSP_up)[order_estimates]
  mu_hat_average = ((mu_postOBSP_up + mu_postOBSP_do) / 2)[order_estimates]
  
  if (is.null(y_axis_lim)) {
    y_axis_lim = c(min(mu_postOBSP_do),
                   1.5 * max(mu_postOBSP_up))
  } 
  
  
  data_plot = data.frame(mu_hat_average,
                         mu_postOBSP_do,
                         mu_postOBSP_up,
                         x_plot)
  
  plot_postOBSP <-
    ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
    coord_cartesian(ylim = y_axis_lim,
                    xlim = c(min(x_plot), max(x_plot))) +
    geom_errorbar(
      aes(
        ymin = mu_postOBSP_do,
        ymax = mu_postOBSP_up,
        color = "post-OBSP"
      ),
      width = 0.8,
      lwd = 1.5
    ) +
    labs(y = ylab, x = xlab) + theme_bw() +
    scale_color_manual(
      name = " ",
      values = c("post-OBSP" = col),
      labels = c(expression("post-OBSP"))
    ) +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11, hjust = 0),
      legend.title = element_blank(),
      legend.position = legend_position
    )
  
  plot_postOBSP
  
}
