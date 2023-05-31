#' Title Estimate hat Y(eta) in Study 1
#'
#' @param y the observed response
#' @param D the treatment variable
#' @param w the covriates W
#' @param wt the related arguments
#'
#' @return the value of estimated y(eta)
#' @export
Est_ycate <- function(y,D,w,wt){
hat_p1 <- fitted(glm(D~w, family = binomial("logit")))
hat_m1<-fitted(lm(y~wt,weights = D))
hat_m0<-fitted(lm(y~wt,weights = 1-D))
y_eta=(D * (y-hat_m1) / hat_p1)- ((1 - D) *(y-hat_m0) / (1 - hat_p1))+hat_m1-hat_m0
return(y_eta)
}
