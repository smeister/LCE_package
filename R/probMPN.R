#' Probability function
#'
#' Calculate the probability to obtain a specific MPN value
#' @param mu getMPN output
#' @param dataMPN dataframe (or list) named "dataMPN": dataMPN<-data.frame(x=..., n=..., v=...)
#' @return the probability over 1
#' @export
probMPN <- function(mu,y,n,v){return(signif(prod(dbinom(y, n, 1-exp(-mu*v))),digits=3))}
