#' Title Estiamte the Y(eta) in TWA application
#'
#' @param y the observed response
#' @param D the treatment variable
#' @param w the covriates W
#' @param wt the related arguments
#'
#' @return the value of estimated y(eta)
#' @export
Est_ytwa <- function(y,D,w,wt){
  hat_p1 <- fitted(glm(D~w, family = binomial("logit")))
  hat_m1<-fitted(glm( y ~ w,weights =D, family = binomial("logit")))
  hat_m0<-fitted(glm( y ~ w,weights =1-D, family = binomial("logit")))
  y_eta=(D * (y-hat_m1) / hat_p1)- ((1 - D) *(y-hat_m0) / (1 - hat_p1))+hat_m1-hat_m0
  return(y_eta)
}
