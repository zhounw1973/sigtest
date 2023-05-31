#' Title: estiamtion of the test statistic
#'
#' @param hat_res the estimated e(hat eta)=hat Y(hat eta)-hat g(X)
#' @param n the sample size
#' @param w the covariates W
#' @param p_a the value of parameter gamma in Tn
#' @param hat_f the nonparametric estimator of f(X)
#'
#' @return the value of test statistic
#' @export
Est_Tn<- function(hat_res,n,w,p_a,hat_f){
  T_n=c(0)
  hat_res=as.matrix(hat_res)
  hat_f=as.matrix(hat_f)
  xw=(1+0.5*as.matrix(dist(w, method = "euclidean", diag = T))^2)^p_a
  xw2=1/(xw)
  xres=(hat_res*hat_f)%*%t(hat_res*hat_f)
  t_n=(sum(xw2*xres)-sum(diag(xres)))
  T_n=(1/(n*(n-1)))*(t_n)
  return(T_n)
}
