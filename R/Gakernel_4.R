

#' Title high-order Guassian kernel
#'

#' @param x the covariate
#'
#' @import stats
#' @return the nonparametric estimator
#' @export
#'
Gakernel_4<-function(x) {
  wx1=(1/2)*(3-x^2)*dnorm(x)
}
