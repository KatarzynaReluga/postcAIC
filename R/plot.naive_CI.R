#' Plots naive confidence intervals (CI) for mixed effects
#'
#' @param x An object of class \code{naive_CI}
#' @param col Colors of CI. Default: \code{NULL}
#' @param xlab Label for the x axis. Default: \code{NULL}
#' @param ylab Label for the y axis. Default: \code{NULL}
#' @param type Type of CI (using first order, second order or
#' both MSE estimators)
#' @param order_estimates Order of intervals in the plot.
#' Default: \code{NULL}
#' @param legend_position Legend position. Default: \code{NULL}
#' @param y_axis_lim Limits of the y axis. Default: \code{NULL}
#' @param ... Additional parameters
#'
#' @return 
#' * Plot with confidence intervals for mixed effects. 
#'
#'
#' @details
#' The output changes slightly depending on the value
#' of the parameter \code{type}. If \code{type = "regular"},
#' naive second-order correct CIs are not present.
#' If \code{type = "corrected"}, naive first-order correct CIS are not present.
#' If \code{type = "both"}, naive first- and second-order correct CIs are present.
#'
#' @importFrom ggplot2 ggplot aes coord_cartesian
#' geom_errorbar labs theme_bw scale_color_manual
#' theme element_text element_blank
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics plot
#'
#' @export
#'

plot.naive_CI <- function(x,
                          col = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          type = c("regular", "corrected", "both"),
                          order_estimates = NULL,
                          legend_position = NULL,
                          y_axis_lim = NULL, 
                          ...) {
  type = match.arg(type)
  
  if (is.null(xlab)) {
    xlab = "ID Cluster"
  }
  
  if (is.null(ylab)) {
    ylab = "Mixed Effect"
  }

  if (is.null(legend_position)) {
    legend_position = c(0.88, 0.88)
  }  
  
  
  if (type == "regular") {
    if (is.null(col)) {
      col = "blue"
    }
    
    x_plot = 1:length(x$mixed_naive_CI_up)
    if (is.null(order_estimates)) {
      order_estimates = x_plot
    }
    mu_naive_do = (x$mixed_naive_CI_do)[order_estimates]
    mu_naive_up = (x$mixed_naive_CI_up)[order_estimates]
    
    if (is.null(y_axis_lim)) {
      y_axis_lim = c(min(mu_naive_do),
                     1.5 * max(mu_naive_up))
    } 
    
    mu_hat_average = ((mu_naive_up + mu_naive_do) / 2)[order_estimates]
    data_plot = data.frame(mu_hat_average,
                           mu_naive_do,
                           mu_naive_up,
                           x_plot)
    
    plot_naive <-
      ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
      coord_cartesian(ylim = y_axis_lim,
                      xlim = c(min(x_plot), max(x_plot))) +
      geom_errorbar(
        aes(
          ymin = mu_naive_do,
          ymax = mu_naive_up,
          color = "Naive"
        ),
        width = 0.8,
        lwd = 1.5
      ) +
      labs(y = ylab, x = xlab) + theme_bw() +
      scale_color_manual(
        name = " ",
        values = c("Naive" = col),
        labels = c(expression("Naive"))
      ) +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0),
        legend.title = element_blank(),
        legend.position = legend_position
      )
    
    plot_naive
    
  } else if (type == "corrected") {
    if (is.null(col)) {
      col = "blue"
    }
    
    x_plot = 1:length(x$mixed_naive_CI_corrected_up)
    if (is.null(order_estimates)) {
      order_estimates = x_plot
    }
    mu_naive_do = (x$mixed_naive_CI_corrected_do)[order_estimates]
    mu_naive_up = (x$mixed_naive_CI_corrected_up)[order_estimates]
    
    if (is.null(y_axis_lim)) {
      y_axis_lim = c(min(mu_naive_do),
                     1.5 * max(mu_naive_up))
    } 
    
    mu_hat_average = ((mu_naive_up +
                         mu_naive_do) / 2)[order_estimates]
    
    data_plot = data.frame(mu_hat_average,
                           mu_naive_do,
                           mu_naive_up,
                           x_plot)
    
    plot_naive <-
      ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
      coord_cartesian(ylim = y_axis_lim,
                      xlim = c(min(x_plot), max(x_plot))) +
      geom_errorbar(
        aes(
          ymin = mu_naive_do,
          ymax = mu_naive_up,
          color = "Naive Cor."
        ),
        width = 0.8,
        lwd = 1.5
      ) +
      labs(y = ylab, x = xlab) + theme_bw() +
      scale_color_manual(
        name = " ",
        values = c("Naive Cor." = col),
        labels = c(expression("Naive Cor."))
      ) +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0),
        legend.title = element_blank(),
        legend.position = legend_position
      )
    
    plot_naive
    
  } else {
    if (is.null(col)) {
      col = brewer.pal(3, name = "Dark2")
    }
    
    x_plot = 1:length(x$mixed_naive_CI_corrected_up)
    
    if (is.null(order_estimates)) {
      order_estimates = x_plot
    }
    
    mu_naive_do = (x$mixed_naive_CI_do)[order_estimates]
    mu_naive_up = (x$mixed_naive_CI_up)[order_estimates]
    mu_naive2_do = (x$mixed_naive_CI_corrected_do)[order_estimates]
    mu_naive2_up = (x$mixed_naive_CI_corrected_up)[order_estimates]
    
    if (is.null(y_axis_lim)) {
      y_axis_lim = c(min(mu_naive_do),
                     1.6 * max(mu_naive_up))
    } 
    
    mu_hat_average = ((mu_naive_up +
                         mu_naive_do) / 2)[order_estimates]
    
    data_plot = data.frame(mu_hat_average,
                           mu_naive_do,
                           mu_naive_up,
                           mu_naive2_do,
                           mu_naive2_up,
                           x_plot)
    
    plot_naive <-
      ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
      coord_cartesian(ylim = y_axis_lim,
                      xlim = c(min(x_plot), max(x_plot))) +
      geom_errorbar(
        aes(
          ymin = mu_naive2_do,
          ymax = mu_naive2_up,
          color = "Naive Cor."
        ),
        width = 1,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_naive_do,
          ymax = mu_naive_up,
          color = "Naive"
        ),
        width = 0.75,
        lwd = 1.5
      ) +
      
      labs(y = ylab, x = xlab) + theme_bw() +
      scale_color_manual(
        name = " ",
        values = c("Naive Cor." = col[1],
                   "Naive" = col[2]),
        labels = c(expression("Naive Cor."),
                   expression("Naive"))
      ) +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0),
        legend.title = element_blank(),
        legend.position = legend_position
      )
    
    plot_naive
  }
  
  
}
