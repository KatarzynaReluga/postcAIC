#' Create matrix Z
#'
#' Function \code{create_Z} creates matrix Z which contains
#' covariates of random effects
#'
#' @param clusterID Vector with cluster labels
#' @param model Type of mixed model: NERM, FHM, RIRS (random slopes and random intercepts).
#' @param nrandom Which slopes should be random? Default: 2
#' @param X Matrix with fixed effects covariates
#' @param intercept Is column of ones, representing the intercept, present in X
#' Default: intercept = TRUE
#'
#' @return Z matrix of covariates for random effects
#'
#'
#' @details 
#' Matrix X is only needed if we select an option model = "RIRS" 
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
#' Z = create_Z("RIRS", clusterID, nrandom = 4, X, intercept = FALSE)
#'
#' @importFrom data.table rbindlist data.table
#'
#' @export
#'
create_Z <- function(model = c("NERM", "FHM", "RIRS"),
                     clusterID,
                     nrandom = 2,  X = NULL,
                     intercept = TRUE) {
  model <- match.arg(model)

  if (is.factor(clusterID)== TRUE) {
    clusterID
  } else { clusterID  = sort(factor(clusterID))
  }

  n_cluster = nlevels(clusterID)


  if (model == "NERM") {
    n_cluster_units = as.data.frame(table(clusterID))$Freq
    cluster_list <- list()

    for (i in 1 : n_cluster) {
      mat_i <- matrix(0, nrow = n_cluster_units[i],
                      ncol = n_cluster)
      mat_i[, i] <- 1

      cluster_list[[i]] <- data.table(mat_i)
    }

    Z <- as.matrix(rbindlist(cluster_list))

  } else if (model == "FHM") {
    Z = diag(n_cluster)
  } else {
    n_cluster_units = as.data.frame(table(clusterID))$Freq
    cluster_list <- list()

    if (!intercept) {
      X = cbind(rep(1, NROW(X)), X)
    }
    p = ncol(X)
    stopifnot("Value of 'nrandom' must be 1 < nrandom <= p.
              Choose model 'NERM' for 'nrandom' = 1" = nrandom > 1 & nrandom <= p)

    stopifnot("X cannot be empty for model 'RS' " = is.null(X) != 1)

    for (i in 1 : n_cluster) {
      mat_i <- matrix(0, nrow = n_cluster_units[i],
                      ncol = n_cluster * nrandom)
      down <- (i-1) * nrandom + 1
      up <- (i-1) * nrandom + nrandom
      mat_i[, c(down:up)] <- 1

      cluster_list[[i]] <- data.table(mat_i)
    }

    Z <- as.matrix(rbindlist(cluster_list))

  }

  Z
}

