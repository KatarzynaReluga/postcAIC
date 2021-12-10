#' Covariates to run simulations
#'
#' Fixed effect covariates with 15 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 75 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 15 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n15_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 150 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 30 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n30_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 300 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 60 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n60_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 450 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 90 * 5, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n90_mi5"

#' Covariates to run simulations
#'
#' Fixed effect covariates with 30 cluster and 5 units in each
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
#' Fixed effect covariates with 30 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data matrix with 600 rows and 4 variables generated
#' from the multivariate normal distribution
#' \code{X = rmvn(n = 30 * 20, mu = rep(0, 4), V = Omega)}
#' where \code{Omega} is a variance covariance matrix with 1 on the diagonal
#' and 0.25 elsewhere
"simulations_n30_mi20"


#' Subset of NHAENS data from R pacakge NHAENS
#'
#' Fixed effect covariates with 30 cluster and 5 units in each
#' cluster to run simulations in Claeskens, Reluga and Sperlich (2021)
#'
#'
#' @format A data frame with 3009 rows and 7 variables:
#' \describe{
#' \item{\code{PhysActive}}{Participant does sport, fitness or recreational activities (Yes or No).}
#' \item{\code{CurrentSmokingStatus}}{Derived variable indicating if person is smoking (1) or is not smoking}
#' \item{\code{Diabetes}}{Is person suffering from diabetes?}
#' \item{\code{SleepHrsNight}}{NUmber of sleeping hours}
#' \item{\code{BPSys2}}{Systolic blood pressure (numeric value)}
#' \item{\code{DirectChol}}{Level of direct cholesterol}
#' \item{\code{Poverty}}{Numerci indicator with smaller values indicating more poverty}
#' \item{\code{clusterID}}{Cluster indicator. Cross-section of varaibles age, biological sex and entinicity. }
#' \item{\code{log_BMI}}{Natuarl logartihm of Body Mass Index (BMI)}
#' }
#' 
#' @source \url{https://cran.r-project.org/web/packages/NHANES/}
"postcAIC_nhaens"
