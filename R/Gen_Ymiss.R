####------function: Gen_Ymiss------####
#' Title: generating the observed response Y and the related arguments
#'
#' @param rep1  the indicator of rep1-th simulation run
#' @param n the sample size
#' @param Type a (i,j) vector which is the indicator of setting
#' @param p the dimension of W
#' @param w the covariates W
#' @param D the missing variable D
#' @param a the dispersion parameter a
#'
#' @return the values of response Y and the related arguments
#' @export
Gen_Ymiss<-function(rep1,n,Type,p,w,D,a){
  set.seed(rep1)
  tg=Type[1]
  td=Type[2]
  gxc=g_x_s1(w,tg)
  gx=gxc[,1]
  if(a!=0){
    dwc=(d_w_s1(w,p,td))
    dw=(dwc[,1])
    wt=cbind(w,gxc[,-1] ,dwc[,-1])
  }else{
    dw=0*gx
    wt=cbind(w,gxc[,-1])
  }
  y<-gx+a*dw+rnorm(n,0,1)
  return(cbind(y,wt))
}
