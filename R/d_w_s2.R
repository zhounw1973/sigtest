####------function: d_w------####
#' Title: Generating the Function Delta(W) in Study 1 under SCenario 2
#'
#' @param w the covariates W
#' @param p the dimension of W
#' @param td the indicator of setting
#'
#' @return the values of Delta(W) and the related arguments
#' @export
d_w_s2<-function(w,p,td){
  zt=w[,-1]
  z <- zt[,-1]
  if(td==1){
    if(ncol(z)!=1){
      dw<-(rowSums(z)) / sqrt(p-2)
    }else{
      dw<-z / sqrt(p-2)
    }
    wt=c()
  }
  if(td==2){
    if(ncol(z)!=1){
      dw<-rowSums(z^3)
    }else{
      dw<-z^3
    }
    wt=(z^3)
  }
  return(cbind(dw,wt))
}
