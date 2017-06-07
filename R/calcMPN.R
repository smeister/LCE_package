#' @export
calcMPN <- function (df) {
  new_df2<-df
  prob_v<-c()
  mpn_v<-c()
  se_v<-c()
  w<-3
  while (w <= length(new_df2)) {
    dataMPN<-data.frame(x=as.numeric(new_df2[[w]]), n=as.numeric(new_df2[[2]]), v=as.numeric(as.character(new_df2[[1]])))
    MPN<-getMPN(y=as.numeric(new_df2[[w]]), n=as.numeric(new_df2[[2]]), v=as.numeric(as.character(new_df2[[1]])))
    prob_v<-c(prob_v, probMPN(MPN, y=as.numeric(new_df2[[w]]), n=as.numeric(new_df2[[2]]), v=as.numeric(as.character(new_df2[[1]]))))
    mpn_v<-c(mpn_v, MPN)
    se_v<-c(se_v, ciMPN(MPN, y=as.numeric(new_df2[[w]]), n=as.numeric(new_df2[[2]]), v=as.numeric(as.character(new_df2[[1]]))))
    w<-w+1
    print(MPN)
  }
  Analysis <- c("MPN", "SE", "Prob")
  v <- c("","","")
  results<-data.frame(cbind(Analysis, v, data.frame(rbind(as.vector(mpn_v), as.vector(se_v), as.vector(prob_v)), stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
  print(results)
  return(results)
}

