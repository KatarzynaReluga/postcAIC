#' Covariates to run simulations
#'
#' Fixed effect covariates with 15 clusters and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#' @format A data matrix with 75 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 15 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere.
"simulations_n15_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 clusters and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#' @format A data matrix with 150 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 30 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere.
"simulations_n30_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 60 clusters and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#' @format A data matrix with 300 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 60 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n60_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 90 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#' @format A data matrix with 450 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 90 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n90_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 cluster and 10 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 300 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 30 * 10, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n30_mi10"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 cluster and 20 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 600 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 30 * 20, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n30_mi20"


#' Subset of NHAENS data from R package NHAENS
#'
#' Data set used in Claeskens, Reluga and Sperlich (2021). 
#' The data set can be found in 
#' \url{https://github.com/KatarzynaReluga/postcAIC/data}
#' The code to create it can be found in 
#' \url{https://github.com/KatarzynaReluga/postcAIC/data_raw}
#'
#' @format A data frame with 3009 rows and 9 columns:
#' \describe{
#' \item{\code{PhysActive}}{Does participant do sport, fitness or recreational activities? (Yes or No)}
#' \item{\code{CurrentSmokingStatus}}{Derived variable indicating if person is smoking (1) or is not smoking (0)}
#' \item{\code{Diabetes}}{Is person suffering from diabetes? (Yes or No)}
#' \item{\code{SleepHrsNight}}{Number of sleeping hours (numeric value)}
#' \item{\code{BPSys2}}{Systolic blood pressure (numeric value)}
#' \item{\code{DirectChol}}{Level of direct cholesterol (numeric value)}
#' \item{\code{Poverty}}{Numeric indicator with smaller values indicating more poverty}
#' \item{\code{clusterID}}{Cluster indicator. Cross-section of variables age, biological sex and entinicity}
#' \item{\code{log_BMI}}{Natural logarithm of Body Mass Index (BMI)}
#' }
#'
#' @source \url{https://cran.r-project.org/web/packages/NHANES/}
"postcAIC_nhaens"
