#' Create matrix Z
#'
#' Function \code{create_Z} creates matrix Z which contains
#' covariates of random effects
#'
#' @param clusterID Vector with cluster labels
#' @param model Type of mixed model: NERM, FHM, RIRS (random slopes and random intercepts)
#' @param nrandom Which slopes should be random? Default: \code{1}
#' @param X Matrix with covariates for fixed effects. Default: \code{NULL}
#' @param intercept Is column of ones, representing the intercept, present in X
#' Default: \code{TRUE}
#'
#' @return 
#' \item{Z}{Matrix of covariates for random effects}
#'
#' @details
#' Matrix X is only needed if we select an option \code{model = "RIRS"}.
#'
#' @examples
#'
#' #NERM
#' clusterID = rep(1:5, 2:6)
#' Z = create_Z("NERM", clusterID)
#'
#' #FHM
#' clusterID = c(1:5)
#' Z = create_Z("FHM", clusterID)
#'
#' #RIRS
#' clusterID = rep(1:5, 10)
#' n = 50
#' p = 10
#' X = matrix(rnorm(n * p), n, p)
#' Z = create_Z("RIRS", clusterID, nrandom = 4,
#'              X, intercept = FALSE)
#'
#' @importFrom data.table rbindlist data.table
#'
#' @export
#'

create_Z <- function(model = c("NERM", "FHM", "RIRS"),
                     clusterID,
                     nrandom = 1,
                     X = NULL,
                     intercept = TRUE) {
  model <- match.arg(model)
  
  if (is.factor(clusterID) == TRUE) {
    clusterID
  } else {
    clusterID  = sort(factor(clusterID))
  }
  
  n_cluster = nlevels(clusterID)
  
  if (model == "FHM") {
    Z = diag(n_cluster)
  } else {
    n_cluster_units = as.data.frame(table(clusterID))$Freq
    params_list  = list()
    
    for (i in 1:n_cluster) {
      params_list[[i]] = list(id_cluster = i,
                              n_units = n_cluster_units[i])
    }
    
    if (model == "RIRS") {
      if (!intercept) {
        X = cbind(rep(1, NROW(X)), X)
      }
      p = ncol(X)
      stopifnot(
        "Value of 'nrandom' must be 1 < nrandom <= p.
              Choose model 'NERM' for 'nrandom' = 1" = nrandom > 1 &
          nrandom <= p
      )
      
      stopifnot("X cannot be empty for model 'RS' " = is.null(X) != 1)
    }
    
    cluster_list  = lapply(params_list, cluster_matrix,
                           n_cluster, nrandom)
    
    Z <- do.call(rbind, cluster_list)
  }
  Z
  
}
