#' Format results to plot
#' 
#' @param x An object of class \code{naive_CI}
#' @param y An object of class \code{postcAIC_CI}
#' @param z An object of class \code{postOBSP_CI}
#' @param type Type of naive CI (using first order, second order or
#' both MSE estimators)
#' 
#' @export
#'

format_results <- function(x, y, z, 
                          type = c("regular", "corrected", "both")) {
  type = match.arg(type)
  
  if (type == "regular") {

    mixed_naive_CI_do = x$mixed_naive_CI_do
    mixed_naive_CI_up = x$mixed_naive_CI_up
    mixed_postcAIC_CI_do = y$mixed_postcAIC_CI_do
    mixed_postcAIC_CI_up = y$mixed_postcAIC_CI_up
    postOBSP_do = z$postOBSP_do
    postOBSP_up = z$postOBSP_up
    
    output  = list(mixed_naive_CI_do = mixed_naive_CI_do, 
                   mixed_naive_CI_up = mixed_naive_CI_up, 
                   mixed_postcAIC_CI_do = mixed_postcAIC_CI_do, 
                   mixed_postcAIC_CI_up = mixed_postcAIC_CI_up, 
                   postOBSP_do = postOBSP_do, 
                   postOBSP_up = postOBSP_up
    )
    
    class(output) <- "multi_CI"
    output
    
  } else if (type == "corrected") {
    mixed_naive_CI_corrected_do = x$mixed_naive_CI_corrected_do
    mixed_naive_CI_corrected_up = x$mixed_naive_CI_corrected_up
    mixed_postcAIC_CI_do = y$mixed_postcAIC_CI_do
    mixed_postcAIC_CI_up = y$mixed_postcAIC_CI_up
    postOBSP_do = z$postOBSP_do
    postOBSP_up = z$postOBSP_up
    
    output  = list(mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do, 
                   mixed_naive_CI_corrected_up = mixed_naive_CI_corrected_up, 
                   mixed_postcAIC_CI_do = mixed_postcAIC_CI_do, 
                   mixed_postcAIC_CI_up = mixed_postcAIC_CI_up, 
                   postOBSP_do = postOBSP_do, 
                   postOBSP_up = postOBSP_up
    )
    
    class(output) <- "multi_CI"
    output
    
  } else {
    
    mixed_naive_CI_do = x$mixed_naive_CI_do
    mixed_naive_CI_up = x$mixed_naive_CI_up
    mixed_naive_CI_corrected_do = x$mixed_naive_CI_corrected_do
    mixed_naive_CI_corrected_up = x$mixed_naive_CI_corrected_up
    mixed_postcAIC_CI_do = y$mixed_postcAIC_CI_do
    mixed_postcAIC_CI_up = y$mixed_postcAIC_CI_up
    postOBSP_do = z$postOBSP_do
    postOBSP_up = z$postOBSP_up
    
    output  = list(mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do, 
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
}}
