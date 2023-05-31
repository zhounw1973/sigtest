
#' Title   Significance Test for ACTG193A in Real Data Analysis
#'
#' @param hc   the parameter related to the bandwidth
#' @param p_a  this parameter is gamma of Tn
#' @param type the indicator of different hypothesis H_0^type
#' @param G1 the comparison with Group G1 and Group 4
#' @param B the number of bootstrap iteration
#' @param alpha the significance level
#'
#' @return the  results for ACTG193A in Real Data Analysis
#' @export
#'
app_ACTG_test<- function(hc,p_a,type,G1,B,alpha){
  load("data/ACTG193A.RData")
  id_weak <- which((ACTG193A$week>=29&ACTG193A$week<=35)|ACTG193A$week==0)
  MMa <- ACTG193A[id_weak,]
  ma1=MMa[MMa$treatment==G1,]
  ma2=MMa[MMa$treatment==4,]
  ma=rbind(ma1,ma2)
  tid <-which(duplicated(ma$id)==1)
  mma <- ma[tid,]
  mma0 <-ma[tid-1,]
  logcd40 <- mma0$logCD4
  ma <- cbind(mma[,-5],logcd40)
  ma$treatment[ma$treatment==G1]=0
  ma$treatment[ma$treatment==4]=1
  D <-ma$treatment
  y <- ma$logCD4
  n <- nrow(ma)
  w <- cbind(ma$age,ma$logcd40)
  k <- ncol(w)
  if(type==1){
    x=ma$logcd40
  }
  if(type==2){
    x=ma$age
  }
  if(type==3)
  {x=w}
  w <- as.matrix(w)
  wt <- w
  ########## estiamte nusanious parameter
  y_eta <- Est_ycate(y,D,w,wt)
  hat_est <- Est_res_f(n,x,hc,y_eta)
  hat_f <- hat_est[,1]
  hat_res <- hat_est[,2]
  ########## estiamte the p-value
  est_pvalue <- Est_pvalue(n,B,wt,w,y,p_a,hat_res,hat_f,x,hc,alpha)
  pvalue <- est_pvalue[2]
  print(c(type,n,B,hc,p_a,pvalue))
  return(pvalue)
}
