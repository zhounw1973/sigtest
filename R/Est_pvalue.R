
#' Title Calculate the empirical p-value
#'
#' @param n the sample size
#' @param B the number of bootstrap iterations
#' @param wt the arguments related to respons
#' @param w the covariates W
#' @param y the observed response#'
#' @param p_a the value of parameter gamma in Tn
#' @param hat_res the estimator of residual
#' @param hat_f the estimator of f(x)
#' @param x the covariates x
#' @param hc the parameter of the bandwidth
#' @param alpha the significance level alpha
#'
#' @import stats
#' @return the estimator of P-value
#' @export
Est_pvalue<- function(n,B,wt,w,y,p_a,hat_res,hat_f,x,hc,alpha){
  T_n=Est_Tn(hat_res,n,w,p_a,hat_f)
  ####################################### estimate critic value ############
  T_nB=sapply(1:B,function(Bi){
    set.seed(Bi+20230520)
    prob = (-1+sqrt(5))/(2*sqrt(5))
    Z =  rbinom(n, 1,prob)
    V = sqrt(5)*Z+(1-sqrt(5))/2
    tilde_err = hat_res*V
    tilde_y=hat_f+tilde_err
    ################### Boostrap ####################
    tilde_est <- Est_res_f(n,x,hc,tilde_y)
    tilde_res <- tilde_est[,2]
    T_b=Est_Tn(tilde_res,n,w,p_a,hat_f)
    return(T_b)
  })
  ################### calcuate the critical value #############
  T_sort <-  sort(T_nB)
  critic <-  T_sort[floor((1-alpha)*B)]
  pvalue <- mean(T_nB>=T_n)
  return(cbind(as.numeric(T_n>critic),pvalue))
}

