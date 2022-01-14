#' Compute matrix upsilon
#'
#' Function \code{compute_upsilon} computes extended selection
#' matrix upsilon
#'
#' @param modelset_matrix Matrix composed of zeros and ones.
#' 
#' @return List with elements:
#' * \code{upsilon} - extended selection matrix, 
#' see Claesknes, Reluga and Sperlich (2021) 
#' * \code{cov_terms_index0} - list with indices of covariance terms
#' in each model
#' * \code{index_full_model} - index of the full model in the model set
#' 
#' @importFrom dplyr select arrange
#' @importFrom utils combn
#'
#' @examples
#' modelset_matrix  = create_modelset("all_subsets", p = 5)
#' upsilon_example = compute_upsilon(modelset_matrix)
#'
#' @export
#'

compute_upsilon <- function(modelset_matrix) {
  p = ncol(modelset_matrix)
  
  # Preparatory steps to construct matrix upsilon ----------------
  index_cov  = which(apply(modelset_matrix, 1, sum) != 1)
  index_no_cov  = which(apply(modelset_matrix, 1, sum) == 1)
  index_cov_no_cov_order = c(index_cov, index_no_cov)
  index_full_model = which.max(apply(modelset_matrix[index_cov, ], 1, sum))
  
  models_more_1_cov  = modelset_matrix[index_cov, ]
  
  cov_terms0 = apply(models_more_1_cov, 1, function(x)
    which(x != 0))
  cov_terms = lapply(cov_terms0, function(x)
    combn(x, 2))
  
  cov_terms_count  = choose(p, 2)
  cov_terms_index0 = lapply(cov_terms, function(x)
    strtoi(paste(x[1, ], x[2, ], sep = "")))
  cov_terms_index = lapply(cov_terms_index0,
                           function(x)
                             which(cov_terms_index0[[index_full_model]] %in% x))
  
  vec0 = numeric(cov_terms_count)
  
  final_cov_index =  lapply(cov_terms_index, ind_nu, vec0 = numeric(cov_terms_count))
  
  upsilon0 <- cbind(modelset_matrix,
                    rbind(
                      matrix(
                        unlist(final_cov_index),
                        nrow = length(final_cov_index),
                        ncol = cov_terms_count,
                        byrow = T
                      ),
                      matrix(0, nrow = length(index_no_cov),
                             ncol = cov_terms_count)
                    ))
  
  # Compute upsilon -----------------------------------------
  upsilon  = as.matrix(select(
    arrange(data.frame(upsilon0, index_cov_no_cov_order),
            by = index_cov_no_cov_order),
    -index_cov_no_cov_order
  ))
  
  
  output  = list(
    upsilon = upsilon,
    cov_terms_index0 = cov_terms_index0,
    index_full_model = index_full_model
  )
  output
  
}
