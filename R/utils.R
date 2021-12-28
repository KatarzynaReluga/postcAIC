#
# This file is for the low level reusable utility functions
# that are not supposed to be visible to a user.
#

#
# General helper utilities ----------------------------------------------------
#


#
#validate matrix
#
validate_matrix <- function(X, allow.na = FALSE) {
  valid.classes <- c("matrix", "data.frame")

  if (!inherits(X, valid.classes)) {
    stop(paste(
      "The only supported data input types are:",
      "`matrix`, `data.frame`"
    ))
  }

  has.missing.values <- any(is.na(X))

  if (!allow.na && has.missing.values) {
    stop("The feature matrix X contains at least one NA.")
  }

  X
}

#
#validate observations
#

validate_observations <- function(V, X, cluster = FALSE) {

  if (any(is.na(V))) {
    stop("The vector of observations (Y, Z or pi_score) contains at least one NA.")
  }

  if (NROW(V)!= nrow(X)) {
    stop("length of observation (Y, Z or pi_score) does not equal nrow(X).")
  }

  if (cluster) {
    if (is.factor(V)== TRUE) {
      V
    } else {
      V  = factor(V)
    }
  } else {

    if (is.matrix(V) && ncol(V) == 1) {
      V <- as.vector(V)
    } else if (!is.vector(V)) {
      stop("Observations y must be vectors.")
    }

    if (!is.numeric(V) && !is.logical(V)) {
      stop("Observations y must be numeric. ")
    }

  }

  V
}


#
# Format matrix in a suitable way
#

format_data_matrix <- function(data, select_row = 1:nrow(data),
                               select_col = NULL,
                               name_col = "X"){


  if (is.null(select_col)){
    data_new <- data.matrix(data[select_row, ])
  } else {
    data_new <- data.matrix(data[select_row, grep(select_col, colnames(data))])
  }

  if (ncol(data_new) == 1) {
    colnames(data_new) <- paste(name_col)
  } else {
    colnames(data_new) <- paste(name_col, 1:ncol(data_new), sep = "")
  }
  data_new
}

#
# Create different combinations of covariates present in a model
#

combinations <-  function(n){
  stopifnot("Length of 'common' cannot be larger than p " = n > 0)
  comb = NULL
  for(i in 1:n) comb = rbind(cbind(1,comb),cbind(0,comb))
  return(comb)
}

#
# Replace elements of vector with 1
#

ind_nu = function(x, vec0) {
  vec0[x] <- 1
  vec0
}

#
# Elements to create matrix Z
#

cluster_matrix  <-  function(params = list(id_cluster = 1, n_units = 5),
           n_cluster, nrandom) {
    id_cluster = params$id_cluster
    n_units = params$n_units

    stopifnot("value of 'id_cluster' must be <= n_cluster" = id_cluster <= n_cluster)
    mat_i <- matrix(0, nrow = n_units,
                    ncol = n_cluster * nrandom)
    down <- (id_cluster - 1) * nrandom + 1
    up <- (id_cluster - 1) * nrandom + nrandom
    mat_i[, c(down : up)] <- 1
    mat_i
  }

