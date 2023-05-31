
#' Title Simulation of Significance Test for Missing Data in Study 2
#'
#' @param rep_num the number of simulation iteration
#' @param n  the sample size
#' @param B the number of bootstrap iteration
#' @param alpha the significance level
#' @param hc   the parameter related to the bandwidth
#' @param p_a  this parameter is gamma of Tn
#' @param Type_y the indicator of the model type (i,j)
#' @param p  the dimension of W
#' @param q  the dimension of x
#'
#' @return the simulation results for Missing Data in Study 2
#' @export
#'
test_miss_study2<- function(rep_num,n,B,hc,p_a,p,q,alpha,Type_y){
  record = c()
  for (disp_a in seq(0, 2, by = 0.2)){
    Emp_reject <- c()
    for(rep1 in 1:rep_num){
      est_reject<- process_study2(rep1,n,B,hc,p_a,p,q,alpha,Type_y,disp_a)
      Emp_reject<- rbind(Emp_reject,est_reject)
    }
    res=c(rep_num,mean(Emp_reject),n,B,hc,p_a,p,q,alpha,Type_y,disp_a)
    record <- rbind(record,res)
  }
  colnames(record) = c("rep","p_value","n","B","hc","p_a","p","q","alpha","Type_y","disp")
  return(record)
}
