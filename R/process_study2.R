#' Title Simulation results for Missing Data in rep1-th simulation run
#'
#' @param rep1 the indicator of rep1-th simulation run
#' @param n  the sample size
#' @param B the number of bootstrap iteration#'
#' @param hc   the parameter related to the bandwidth#'
#' @param p_a  this parameter is gamma of Tn#'
#' @param p  the dimension of W
#' @param q  the dimension of x
#' @param alpha the significance level
#' @param Type_y the indicator of the model type (i,j,k)
#' @param disp_a the dispersion parameter a
#' @return the simulation results of rep1-th simulation run
#' @export
process_study2<-function(rep1,n,B,hc,p_a,p,q,alpha,Type_y,disp_a){
  Type <-  rbind(rep(c(1,2),each=2),rep(c(1,2),times=2))
  Ty <- Type[,Type_y]## Ty is the indicator of the model type (i,j,k)
  w <- Gen_W(rep1,n,p) #generate the covariates w
  D <- Gen_D(rep1,arg="s1",n,p,w) #generate the treatment variable D
  ywt <-Gen_Ymiss(rep1,n,Ty,p,w,D,disp_a)  # generate the respons
  y <- ywt[,1]
  wt <- ywt[,-1]
  x=w[,1:q]
  ########## estiamte nusanious parameter
  y_eta <- Est_ymiss(y,D,w,wt)
  hat_est <- Est_res_f(n,x,hc,y_eta)
  hat_f <- hat_est[,1]
  hat_res <- hat_est[,2]
  est_pvalue <- Est_pvalue(n,B,wt,w,y,p_a,hat_res,hat_f,x,hc,alpha) #estiamte the p-value
  return(est_pvalue[1])
}
