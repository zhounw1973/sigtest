####------function: g_x------####
#' Title: generating the function g(X)
#'
#' @param w the covariates W
#' @param tg the indicator of setting
#'
#' @return the values of g(X) and the related arguments
#' @export
g_x_s2<-function(w,tg){
  x=w[,1]
  if(tg==1){
    gx=2*x
    wt=c()
  }
  if(tg==2){
    gx=x^2-2*x
    wt= x^2
  }
  return(cbind(gx,wt))
}
