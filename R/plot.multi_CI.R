#' Plots naive, post-cAIC and post-OBSP CI for mixed effects
#'
#' @param x An object of class \code{multi_CI}
#' @param col Colour of CI
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param type Type of CI (using first order, second order or
#' both MSE estimators)
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

plot.multi_CI <- function(x,
                          col = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          type = c("regular", "corrected", "both"),
                          order_estimates = NULL,
                          ...) {
  type = match.arg(type)

  if (is.null(xlab)) {
    xlab = "ID Cluster"
  }

  if (is.null(ylab)) {
    ylab = "Mixed Effect"
  }
  if (type == "regular") {
    if (is.null(col)) {
      col = brewer.pal(3, name = "Dark2")
    }

    x_plot = 1:length(x$mixed_naive_CI_up)
    if (is.null(order_estimates)) {
      order_estimates = x_plot
    }

    mu_naive_do = (x$mixed_naive_CI_do)[order_estimates]
    mu_naive_up = (x$mixed_naive_CI_up)[order_estimates]
    mu_postcAIC_do = (x$mixed_postcAIC_CI_do)[order_estimates]
    mu_postcAIC_up = (x$mixed_postcAIC_CI_up)[order_estimates]
    mu_postOBSP_do = (x$postOBSP_do)[order_estimates]
    mu_postOBSP_up = (x$postOBSP_up)[order_estimates]

    mu_hat_average = ((mu_naive_up + mu_naive_do) / 2)[order_estimates]
    data_plot = data.frame(
      mu_hat_average,
      mu_naive_do,
      mu_naive_up,
      mu_postOBSP_do,
      mu_postOBSP_up,
      mu_postcAIC_do,
      mu_postcAIC_up,
      x_plot
    )

    plot_multi <-
      ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
      coord_cartesian(ylim = c(min(mu_naive_do),
                               1.5 * max(mu_naive_up)),
                      xlim = c(min(x_plot), max(x_plot))) +
      geom_errorbar(
        aes(
          ymin = mu_postOBSP_do,
          ymax = mu_postOBSP_up,
          color = "Post-OBSP"
        ),
        width = 1,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_postcAIC_do,
          ymax = mu_postcAIC_up,
          color = "Post-cAIC"
        ),
        width = 0.8,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_naive_do,
          ymax = mu_naive_up,
          color = "Naive"
        ),
        width = 0.6,
        lwd = 1.5
      ) +
      labs(y = ylab, x = xlab) + theme_bw() +
      scale_color_manual(
        name = " ",
        values = c(
          "Post-OBSP" = col[1],
          "Post-cAIC" = col[2],
          "Naive" = col[3]
        ),
        labels = c(
          expression("Post-OBSP"),
          expression("Post-cAIC"),
          expression("Naive")
        )
      ) +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.9)
      )

    plot_multi

  } else if (type == "corrected") {
    if (is.null(col)) {
      col = brewer.pal(3, name = "Dark2")
    }

    x_plot = 1:length(x$mixed_naive_CI_up)
    if (is.null(order_estimates)) {
      order_estimates = x_plot
    }

    mu_naive_do = (x$mixed_naive_CI_corrected_do)[order_estimates]
    mu_naive_up = (x$mixed_naive_CI_corrected_up)[order_estimates]
    mu_postcAIC_do = (x$mixed_postcAIC_CI_do)[order_estimates]
    mu_postcAIC_up = (x$mixed_postcAIC_CI_up)[order_estimates]
    mu_postOBSP_do = (x$postOBSP_do)[order_estimates]
    mu_postOBSP_up = (x$postOBSP_up)[order_estimates]

    mu_hat_average = ((mu_naive_up + mu_naive_do) / 2)[order_estimates]
    data_plot = data.frame(
      mu_hat_average,
      mu_naive_do,
      mu_naive_up,
      mu_postOBSP_do,
      mu_postOBSP_up,
      mu_postcAIC_do,
      mu_postcAIC_up,
      x_plot
    )

    plot_multi <-
      ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
      coord_cartesian(ylim = c(min(mu_naive_do),
                               1.5 * max(mu_naive_up)),
                      xlim = c(min(x_plot), max(x_plot))) +
      geom_errorbar(
        aes(
          ymin = mu_postOBSP_do,
          ymax = mu_postOBSP_up,
          color = "Post-OBSP"
        ),
        width = 1,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_postcAIC_do,
          ymax = mu_postcAIC_up,
          color = "Post-cAIC"
        ),
        width = 0.8,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_naive_do,
          ymax = mu_naive_up,
          color = "Naive"
        ),
        width = 0.6,
        lwd = 1.5
      ) +
      labs(y = ylab, x = xlab) + theme_bw() +
      scale_color_manual(
        name = " ",
        values = c(
          "Post-OBSP" = col[1],
          "Post-cAIC" = col[2],
          "Naive" = col[3]
        ),
        labels = c(
          expression("Post-OBSP"),
          expression("Post-cAIC"),
          expression("Naive")
        )
      ) +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.9)
      )

    plot_multi

  } else {
    if (is.null(col)) {
      col = brewer.pal(4, name = "Dark2")
    }

    x_plot = 1:length(x$mixed_naive_CI_up)
    if (is.null(order_estimates)) {
      order_estimates = x_plot
    }

    mu_naive_do = (x$mixed_naive_CI_do)[order_estimates]
    mu_naive_up = (x$mixed_naive_CI_up)[order_estimates]
    mu_naive2_do = (x$mixed_naive_CI_corrected_do)[order_estimates]
    mu_naive2_up = (x$mixed_naive_CI_corrected_up)[order_estimates]
    mu_postcAIC_do = (x$mixed_postcAIC_CI_do)[order_estimates]
    mu_postcAIC_up = (x$mixed_postcAIC_CI_up)[order_estimates]
    mu_postOBSP_do = (x$postOBSP_do)[order_estimates]
    mu_postOBSP_up = (x$postOBSP_up)[order_estimates]

    mu_hat_average = ((mu_naive_up + mu_naive_do) / 2)[order_estimates]
    data_plot = data.frame(
      mu_hat_average,
      mu_naive_do,
      mu_naive_up,
      mu_naive2_do,
      mu_naive2_up,
      mu_postOBSP_do,
      mu_postOBSP_up,
      mu_postcAIC_do,
      mu_postcAIC_up,
      x_plot
    )

    plot_multi <-
      ggplot(data_plot, aes(x = x_plot, y = mu_hat_average)) +
      coord_cartesian(ylim = c(min(mu_naive_do),
                               1.5 * max(mu_naive_up)),
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
          ymin = mu_postOBSP_do,
          ymax = mu_postOBSP_up,
          color = "Post-OBSP"
        ),
        width = 0.85,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_postcAIC_do,
          ymax = mu_postcAIC_up,
          color = "Post-cAIC"
        ),
        width = 0.7,
        lwd = 1.5
      ) +
      geom_errorbar(
        aes(
          ymin = mu_naive_do,
          ymax = mu_naive_up,
          color = "Naive"
        ),
        width = 0.55,
        lwd = 1.5
      ) +
      labs(y = ylab, x = xlab) + theme_bw() +
      scale_color_manual(
        name = " ",
        values = c(
          "Naive Cor." = col[1],
          "Post-OBSP" = col[2],
          "Post-cAIC" = col[3],
          "Naive" = col[4]
        ),
        labels = c(
          expression("Naive Cor."),
          expression("Post-OBSP"),
          expression("Post-cAIC"),
          expression("Naive")
        )
      ) +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.85)
      )

    plot_multi
  }


}
