#' Title Get the Nonparametric Estimator of e(eta_0) and f(x)
#'
#' @param n sample size
#' @param x significant variables
#' @param hc the parameter related to the bandwidth
#' @param y_eta the estimator of y(eta_0)
#'
#' @return the Nonparametric Estimator of e(eta_0) and f(x)
#' @export
Est_res_f <-function(n,x,hc,y_eta){
hat_g= matrix(0, ncol = 1, nrow = length(x))
hat_f= matrix(0, ncol = 1, nrow = length(x))
if(is.vector(x)){
h=hc*sd(x)*n^(-1/4)
for (xi in 1:length(x)) {
  xz <- (x[-xi]- x[xi]) / h
  wx <- dnorm(xz)
  hat_g[xi] = sum(wx * y_eta[-xi]) / sum(wx)
  hat_f[xi]=sum(wx)/(n*h)
}}else{ if(ncol(x)==2){
       h=hc*sum(diag((var(x))^{1/2}))*n^(-1/8)
    for (xi in 1:n) {
      xz1 <- (x[-xi,1]- x[xi,1]) / h
      xz2 <- (x[-xi,2]- x[xi,2]) / h
      wx <-Gakernel_4(xz1)*Gakernel_4(xz2)
      wx[wx<0]=0
      hat_g[xi] = sum(wx * y_eta[-xi]) / sum(wx)
      hat_f[xi]=sum(wx)/(n*h)
     }
  }
}
hat_res=y_eta-hat_g
return(cbind(hat_f,hat_res))
}
