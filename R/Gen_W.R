####------function: Gen_W------####
#' Title generating the covariates W
#'
#' @param rep1 the indicator of rep1-th simulation run
#' @param n the sample size
#' @param p the dimension of W
#'
#' @return the values of covariates W
#' @export
Gen_W<-function(rep1,n,p){
  set.seed(rep1)
  w=c()
  for(j in 1:p){
    w=cbind(w,runif(n,-1,1))
  }
  return(w)
}
