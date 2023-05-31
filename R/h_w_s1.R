####------function: h_w------####
#' Title:generating the function h(W)
#'
#' @param w the covariates W
#' @param th the indicator of setting
#' @param p the dimension of w
#' @return the values of h(W)
#' @export
h_w_s1<-function(w,th,p){
  if(th==1){
    hw=0
  }
  if(th==2){
    hw=(rowSums(w)) / sqrt(p)
  }
  return(hw)
}
