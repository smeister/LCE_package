#' Dilution vector
#'
#' Creates a vector with a starting value of x and length of y and dilution of z each incrementation.
#' @param x A numeric value as starting value
#' @param y A numeric value as vector length
#' @param z A numeric value as division factor
#' @return A vector
#' @export
repDIL <- function (x, y, z) {
  dil<-c(x)
  k<-1
  while (k < y) {
    dil<-append(dil, dil[k]*(1/z))
    k<-k+1
  }
  return(dil)
}
