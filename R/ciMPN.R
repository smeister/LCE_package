#' Standard Error / Confidence Interval function
#'
#' Calculate the Standard Error (SE) or Confidence Interval (LL, UL)
#' @param mu getMPN output
#' @param dataMPN dataframe (or list) named "dataMPN": dataMPN<-data.frame(x=..., n=..., v=...)
#' @param alpha alpha value (usually 0.05 for 95 CI)
#' @param output if FALSE returns the standard error, if TRUE returns the confidence interval
#' @return the standard error/confidence interval
#' @export
ciMPN <- function(mu,y,n,v,alpha=0.05, output=FALSE) {
  SE=(mu^2*sum((n*v^2)/(exp(mu*v)-1)))^-0.5
  LL=signif(exp(log(mu)-qnorm(1-alpha/2,0,1)*SE),digits=3)
  UL=signif(exp(log(mu)+qnorm(1-alpha/2,0,1)*SE),digits=3)
  if (output==TRUE) {
    CI<-c(LL, UL)
    return(CI)
  } else {
    return(SE)
  }
}
