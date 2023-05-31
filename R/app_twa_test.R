
#' Title   Significance Test for TWA in Real Data Analysis
#'
#' @param hc   the parameter related to the bandwidth
#' @param p_a  this parameter is gamma of Tn
#' @param type the indicator of different hypothesis H_0^type
#' @param B the number of bootstrap iteration
#' @param alpha the significance level
#'
#' @return the  results for TWA in Real Data Analysis
#' @export
#'
#'
app_twa_test<- function(hc,p_a,type,B,alpha){
  load("data/tempwork.RData")
  Ma <- tempwork
  ma=Ma[Ma$tosc==1,]
  n <- nrow(ma)
  D <- ma$treat
  y <- ma$perm3
  w <- cbind(ma$male, ma$single, ma$age2, ma$dloc01, ma$dloc02, ma$child ,
             ma$ysch ,ma$pvoto, ma$train1 ,
             ma$nyu1, ma$emp1, ma$unemp1, ma$perm1, ma$blu1, ma$self1 ,ma$manuf1, ma$serv1 ,
             ma$wage1 ,ma$hour1,
             ma$fysch, ma$femp1, ma$fblu,
             ma$dist, ma$dist^2, ma$self1*ma$li)
  k <- ncol(w)
  record_pvalue <-c()
  n <- nrow(ma)
  if(type==1){
    x=ma$dist
  }
  if(type==2){
    x=ma$wage1
  }
  if(type==3)
  {x=as.matrix(cbind(ma$wage1,ma$dist),nrow=n)}
  w <- as.matrix(w)
  wt <- w
  ########## estiamte nusanious parameter
  y_eta <- Est_ytwa(y,D,w,w)
  hat_est <- Est_res_f(n,x,hc,y_eta)
  hat_f <- hat_est[,1]
  hat_res <- hat_est[,2]
  ########## estiamte the p-value
  est_pvalue <- Est_pvalue(n,B,wt,w,y,p_a,hat_res,hat_f,x,hc,alpha)
  pvalue <- est_pvalue[2]
  print(c(type,n,B,hc,p_a,pvalue))
  return(pvalue)
}
