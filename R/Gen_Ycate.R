####------function: Gen_Ycate------####
#' Title: generating the observed response Y and the related arguments
#'
#' @param rep1  the indicator of rep1-th simulation run
#' @param arg  the Indicator of Scenario in Study1
#' @param n the sample size
#' @param Type a (i,j,k) vector which is the indicator of setting
#' @param p the dimension of W
#' @param w the covariates W
#' @param D the treatment variable D
#' @param a the dispersion parameter a
#'
#' @return the values of response Y and the related arguments
#' @export
Gen_Ycate<-function(rep1,arg,n,Type,p,w,D,a){
  set.seed(rep1)
  th=Type[1]
  tg=Type[2]
  td=Type[3]
  if(arg=="s1"){
  hw=h_w_s1(w,th,p)
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
  }
  if(arg=='s2')
  {
    hw=h_w_s2(w,th,p)
    gxc=g_x_s2(w,tg)
    gx=gxc[,1]
    if(a!=0){
      dwc=(d_w_s2(w,p,td))
      dw=(dwc[,1])
      wt=cbind(w,gxc[,-1] ,dwc[,-1])
    }else{
      dw=0*gx
      wt=cbind(w,gxc[,-1])
    }
  }
  y1<-hw+gx+a*dw+rnorm(n,0,1)
  y0<-hw+rnorm(n,0,1)
  y=D*y1+(1-D)*y0
  return(cbind(y,wt))
}
