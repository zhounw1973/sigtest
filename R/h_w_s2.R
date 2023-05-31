####------function: h_w------####
#' Title:generating the function h(W)
#'
#' @param w the covariates W
#' @param th the indicator of setting
#' @param p the dimension of W
#' @return the values of h(W)
#' @export
h_w_s2<-function(w,th,p){
  p <- ncol(w)
  if(th==1){
    hw=0
  }
  if(th==2){
    hw=w[,p]
  }
  return(hw)
}
