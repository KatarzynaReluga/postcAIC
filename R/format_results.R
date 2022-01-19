#' Format results to plot them together
#'
#' Function \code{format_results} creates an object of class \code{multi_CI}
#' to plot different confidence intervals for mixed effects together
#'
#' @param x An object of class \code{naive_CI}
#' @param y An object of class \code{postcAIC_CI}
#' @param z An object of class \code{postOBSP_CI}
#' @param type Type of naive CI (using first order, second order or
#' both MSE estimators)
#'
#' @return List with elements:
#' \item{mixed_postcAIC_CI_up}{Upper boundary of CI for mixed effects}
#' \item{mixed_postcAIC_CI_do}{Lower boundary of CI for mixed effects}
#' \item{postOBSP_up}{Upper boundary of CI for mixed effects}
#' \item{postOBSP_do}{Lower boundary of CI for mixed effects}
#' \item{mixed_naive_CI_corrected_up}{Upper boundary of naive CI for mixed effects with the second order MSE}
#' \item{mixed_naive_CI_corrected_do}{Lower boundary of naive CI for mixed effects with the second order MSE}
#' \item{mixed_naive_CI_up}{Upper boundary of naive CI for mixed effects with the first order MSE}
#' \item{mixed_naive_CI_do}{Lower boundary of naive CI for mixed effects with the first order MSE}
#'
#' @details
#' The output changes slightly depending on the value
#' of the parameter \code{type}. If \code{type = "regular"},
#' naive second-order correct CIs are not present.
#' If \code{type = "corrected"}, naive first-order correct CIS are not present.
#' If \code{type = "both"}, naive first- and second-order correct CIs are present.
#'
#'
#'
#' @export
#'

format_results <- function(x,
                           y,
                           z,
                           type = c("regular", "corrected", "both")) {
  type = match.arg(type)
  
  if (type == "regular") {
    mixed_naive_CI_do = x$mixed_naive_CI_do
    mixed_naive_CI_up = x$mixed_naive_CI_up
    mixed_naive_CI_corrected_do = "Corrected CI for mixed effects not requested"
    mixed_naive_CI_corrected_up = "Corrected CI for mixed effects not requested"
  } else if (type == "corrected") {
    mixed_naive_CI_corrected_do = x$mixed_naive_CI_corrected_do
    mixed_naive_CI_corrected_up = x$mixed_naive_CI_corrected_up
    mixed_naive_CI_up = "Regular CI for mixed effects not requested"
    mixed_naive_CI_do = "Regular CI for mixed effects not requested"
  } else {
    mixed_naive_CI_do = x$mixed_naive_CI_do
    mixed_naive_CI_up = x$mixed_naive_CI_up
    mixed_naive_CI_corrected_do = x$mixed_naive_CI_corrected_do
    mixed_naive_CI_corrected_up = x$mixed_naive_CI_corrected_up
  }
  
  mixed_postcAIC_CI_do = y$mixed_postcAIC_CI_do
  mixed_postcAIC_CI_up = y$mixed_postcAIC_CI_up
  postOBSP_do = z$postOBSP_do
  postOBSP_up = z$postOBSP_up
  
  output  = list(
    mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do,
    mixed_naive_CI_corrected_up = mixed_naive_CI_corrected_up,
    mixed_naive_CI_do = mixed_naive_CI_do,
    mixed_naive_CI_up = mixed_naive_CI_up,
    mixed_postcAIC_CI_do = mixed_postcAIC_CI_do,
    mixed_postcAIC_CI_up = mixed_postcAIC_CI_up,
    postOBSP_do = postOBSP_do,
    postOBSP_up = postOBSP_up
  )
  
  class(output) <- "multi_CI"
  output
}
