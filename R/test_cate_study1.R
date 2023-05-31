
#' Title Simulation of Significance Test for CATE in Study 1
#'
#' @param rep_num the number of simulation iteration
#' @param n  the sample size
#' @param B the number of bootstrap iteration
#' @param alpha the significance level
#' @param arg  the indicator of the Scenario "s1" or "s2"
#' @param hc   the parameter related to the bandwidth
#' @param p_a  this parameter is gamma of Tn
#' @param Type_y the indicator of the model type (i,j,k)
#' @param p  the dimension of W
#' @param q  the dimension of x
#'
#' @return the simulation results for CATE in Study 1
#' @export
#'
test_cate_study1<- function(rep_num,n,B,hc,p_a,p,q,alpha,Type_y,arg){
     record = c()
     for (disp_a in seq(0, 2, by = 0.2)){
       Emp_reject <- c()
        for(rep1 in 1:rep_num){
             est_reject<- process_study1(rep1,n,B,hc,p_a,p,q,alpha,Type_y,disp_a,arg)
             Emp_reject<- rbind(Emp_reject,est_reject)
        }
       res=c(rep_num,mean(Emp_reject),n,B,hc,p_a,p,q,alpha,Type_y,disp_a,arg)
       record <- rbind(record,res)
     }
        colnames(record) = c("rep","p_value","n","B","hc","p_a","p","q","alpha","Type_y","disp","arg")
         return(record)
  }
