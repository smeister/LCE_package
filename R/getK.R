#' GetK function
#'
#' @export
getK=function(d,r,plot=F){ # r is to choose the replicate
  if(missing(r)){
    d=d
  } else{
    d=transform((subset(d,rep==r&!is.na(x))),rep=as.numeric(interaction(rep,drop=T)))
  }

  ur=length(unique(d$rep))
  ut=c();for(i in 1:ur){ut[i]=length(unique(d$t[d$rep==i]))}
  times=matrix(NA,nrow=max(ut),ncol=ur)
  for(i in 1:ur){for(j in 1:ut[i]){times[1:length(unique(d$t[d$rep==i])),i]=unique(d$t[d$rep==i])}}

  MPNs=matrix(NA,nrow=max(ut),ncol=ur)
  lnDiffs=matrix(NA,nrow=max(ut),ncol=ur)
  for(j in 1:ur){
    for(i in 1:max(ut)){
      MPNs[i,j]=getMPN(y=d$x[(d$rep %in% j)&(d$t==times[i])],n=d$n[(d$rep %in% j)&(d$t==times[i])],v=d$v[(d$rep %in% j)&(d$t==times[i])])
      lnDiffs[i,j]=log(MPNs[i,j]/MPNs[1,j])
    }
  }
  MPNs
  lnDiffs
  logDiffs=log10(exp(lnDiffs))
  k_init=-coef(lm(as.vector(lnDiffs)~as.vector(times)))[2]

  #master lnL method
  mu0s=MPNs[1,]
  sum=0
  lnLs=function(x,n,v,k,t,b,mu0){
    for(m in 1:ur){
      x=d$x[(d$rep %in% m)]
      n=d$n[(d$rep %in% m)]
      v=d$v[(d$rep %in% m)]
      t=d$t[(d$rep %in% m)]
      sum=sum+sum((n-x)*(mu0*exp(-k*t+b))*v-x*log(1-exp(-(mu0*exp(-k*t+b))*v)),na.rm=T)
    }
    return(sum)
  }
  Mast2=mle2(lnLs,start=list(k=k_init,b=0,mu0=mu0s[1]),data=d,method="BFGS",optimizer="nlminb",skip.hessian=F)

  # output and return
  if(plot==T){
    plotplot <- function (a, b, c) {
      a
      b
      c
    }
    supergraph <- plotplot(
      par(mar=c(4, 4, 1, 1)) ,plot(times,logDiffs),
      abline(a=coef(Mast2)[2]*log10(exp(1)),b=-coef(Mast2)[1]*log10(exp(1)),lty=1,col='red')
    )


    return(supergraph)

  } else {
    kGot=cbind(confint(Mast2,parm=c("k"),method="quad")[1],coef(Mast2)[1],confint(Mast2,parm=c("k"),method="quad")[2])
    #kGot=cbind(confint(Mast2)[,1],coef(Mast2),confint(Mast2)[,2])
    colnames(kGot)=c("LP 2.5%","MLE","LP 97.5%");rownames(kGot)=c("Decay Rate (k)")
    return(data.frame(kGot))
  }

}

