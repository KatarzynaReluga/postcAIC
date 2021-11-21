#' Create matrix with all models
#'
#' Function \code{create_modelset} creates a matrix indicating
#' covariates (fixed effects) in each model considered by the user
#'
#' @param modelset Type of model set:
#' * \code{"all_subsets"} - all possible models for all parameters
#' * \code{"nested"} - a sequence of nested models for all parameters
#' * \code{"part_subset"} - all possible models for a subset of parameters
#' @param common A vector indicating variables forced to be present in each model
#' @param p Number of all fixed parameters under consideration (intercept included)
#'
#' @return modelset_matrix Matrix composed of zeros and ones. Ones correspond to
#' parameters in a model which is represented in nth row.
#'
#' @details
#' * \code{modelset = "nested"}
#' The first model contains one covariate
#' from the first column of X, then two covariates from the first and the second column
#' of X, etc.
#' *\code{common}
#' A vector of length at most p, consisting of those column numbers of X for which the
#'    variables are forced to be present in every model (e.g. c(2,5) for variables 2 and 5).
#'    If provided, it is used in model.set="partsubsets".
#'
#' @examples
#' #"all_subsets"
#' modelset  = create_modelset("all_subsets", p = 5)
#'
#' #"nested"
#' modelset  = create_modelset("nested", p = 5)
#'
#' #"part_subset"
#' modelset  = create_modelset("part_subset", p = 5,
#'                              common = c(1:3))
#'
#' @export

create_modelset <- function(modelset  = c("all_subsets", "nested",
                                            "part_subset"),
                             common = NULL, p  = 5){

  modelset <- match.arg(modelset)

  if (modelset == 'all_subsets') {
    models0 = combinations(p)
    modelset_matrix = models0[rowSums(models0 != 0) > 0, ]  # no 'empty' model
  } else if (modelset == 'nested') {
    temp_m = matrix(1, p, p)
    temp_m[upper.tri(temp_m, diag = FALSE)] <- 0
    modelset_matrix = temp_m
  } else {
    stopifnot("Parameter 'common' cannot be empty if modelset = 'partsubsets'" = !is.null(common))
    models0 = combinations(p - length(common))
    common_col = matrix(1, nrow(models0), length(common))
    modelset_matrix = cbind(common_col, models0)
  }

  modelset_matrix
}
