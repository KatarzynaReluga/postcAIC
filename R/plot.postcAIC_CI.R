#' Plots post-cAIC CI for mixed effects
#'
#' @param x An object of class \code{postcAIC_CI}
#' @param col Colour of CI
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param order_estimates Order of intervals in the plot
#' @param ... Additional parameters
#'
#' @importFrom ggplot2 ggplot aes coord_cartesian
#' geom_errorbar labs theme_bw scale_color_manual
#' theme element_text element_blank
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics plot
#'
#' @export
#'

plot.postcAIC_CI <- function(x,
                          col = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          order_estimates = NULL,
                          ...) {
  if (is.null(xlab)) {
    xlab = "ID Cluster"
  }

  if (is.null(ylab)) {
    ylab = "Mixed Effect"
  }

  if (is.null(col)) {
    col = "blue"
  }

  x_plot = 1:length(x$mixed_postcAIC_CI_up)
  if (is.null(order_estimates)) {
    order_estimates = x_plot
  }

  mu_postcAIC_do = (x$mixed_postcAIC_CI_do)[order_estimates]
  mu_postcAIC_up = (x$mixed_postcAIC_CI_up)[order_estimates]
  mu_hat_average = ((mu_postcAIC_up + mu_postcAIC_do) / 2)[order_estimates]


  data_plot = data.frame(mu_hat_average,
                         mu_postcAIC_do,
                         mu_postcAIC_up,
                         x_plot)

  plot_postcAIC <-
    ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
    coord_cartesian(ylim = c(min(mu_postcAIC_do),
                             1.5 * max(mu_postcAIC_up)),
                    xlim = c(min(x_plot), max(x_plot))) +
    geom_errorbar(
      aes(
        ymin = mu_postcAIC_do,
        ymax = mu_postcAIC_up,
        color = "Post-cAIC"
      ),
      width = 0.8,
      lwd = 1.5
      ) +
    labs(y = ylab, x = xlab) + theme_bw() +
    scale_color_manual(
      name = " ",
      values = c("Post-cAIC" = col),
      labels = c(expression("Post-cAIC"))
    ) +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11, hjust = 0),
      legend.title = element_blank(),
      legend.position = c(0.85, 0.9)
    )

  plot_postcAIC

  }
