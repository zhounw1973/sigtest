
####------function: Gen_D------####
#' Title: generating the treatment variable D
#'
#' @param rep1  the indicator of rep1-th simulation run
#' @param arg  the indicator of the Scenario "s1" or "s2"
#' @param n the sample size
#' @param p the dimension of W
#' @param w the covariates W
#'
#' @return the values of treatment D
#' @export
Gen_D<-function(rep1,arg,n,p,w){
  set.seed(rep1)
  if(arg=="s1"){
  eta = (rowSums(w)) / sqrt(p)
  }
  if(arg=="s2"){
    wz <- w[,-p]
    pz <- ncol(wz)
    eta = (rowSums(wz)) / sqrt(pz)
  }
  mu = exp(eta) / (1 + exp(eta))
  D = seq(0, 0, length = n)
  for (i in 1:n) {
    D[i] = rbinom(1, 1, mu[i])
  }
  return(D)
}
