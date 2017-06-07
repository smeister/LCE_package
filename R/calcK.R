#' @export
calcK <- function (dataLIST, timeLIST, replicate, graph = FALSE) {
  # Empty vectors
  samp_v<-c() # name of the sample
  brep_v<-c() # biological replicate number
  t_v<-c() # time or dose
  v_v<-c() # dilution
  x_v<-c() # MPN data
  n_v<-c() # MPN replicate number

  ###############################################
  # Add a warning message if time/dose is empty #
  #     Or if there is a NA value somewhere    #
  ###############################################

  r<-1
  while (r <= length(dataLIST)) {
    d<-3
    f<-2
    while (d <= length(dataLIST[[r]])) {
      samp_v <- c(samp_v, rep("sample", length(dataLIST[[r]]$dil.)))
      brep_v <- c(brep_v, rep(r, length(dataLIST[[r]]$dil.)))
      v_v <- c(v_v, dataLIST[[r]]$dil.)
      n_v <- c(n_v, dataLIST[[r]]$rep.)
      t_v <- c(t_v, rep(timeLIST[[r]][[f]], length(dataLIST[[r]]$dil.)))
      x_v <- c(x_v, dataLIST[[r]][[d]])
      d <- d+1
      f <- f+1
    }
    r <- r+1
  }

  # Dataframe to input in getK function
  results <- data.frame(
    samp=samp_v,
    rep=as.numeric(as.character(brep_v)),
    t=as.numeric(as.character(t_v)),
    v=as.numeric(as.character(v_v)),
    x=as.numeric(as.character(x_v)),
    n=as.numeric(as.character(n_v))
  )

  # output and return
  return(getK(results, r = replicate, plot = graph))

}


