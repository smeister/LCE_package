#' regK function
#'
#' @export
regK=function(d,r){
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
      MPNs[i,j]=getMPN(y=d$x[(d$rep %in% j)&(d$t==times[i,j])],n=d$n[(d$rep %in% j)&(d$t==times[i,j])],v=d$v[(d$rep %in% j)&(d$t==times[i,j])])
      lnDiffs[i,j]=log(MPNs[i,j]/MPNs[1,j])
    }
  }
  MPNs
  lnDiffs
  logDiffs=log10(exp(lnDiffs))

  nas=which(is.na(lnDiffs))
  colsNA=floor((nas-1)/nrow(lnDiffs))+1
  rowsNA=nas-(colsNA-1)*nrow(lnDiffs)

  #colNA=which(is.na(lnDiffs[match(NA,lnDiffs),]))
  #rowNA=which(is.na(lnDiffs))
  times[rowsNA,colsNA]=NA;times

  Mast1=lm(as.vector(lnDiffs)~as.vector(times))
  kM1=-coef(Mast1)[2]
  kM1L=-confint(Mast1)[2,2]
  kM1U=-confint(Mast1)[2,1]
  aM1=coef(Mast1)[1]
  aM1L=confint(Mast1)[1,1]
  aM1U=confint(Mast1)[1,2]

  #plot(times,logDiffs,xlim=c(0,max(times,na.rm=T)),ylim=c(min(logDiffs,na.rm=T),max(logDiffs,na.rm=T)))
  C=log10(exp(1))
  Mast1C=lm(as.vector(logDiffs)~as.vector(times))
  #abline(a=aM1*C,b=-kM1*C,lty=1,col='blue')
  #lines(sort(as.vector(times)),sort(as.vector(predict.lm(Mast1C,interval='confidence')[,'lwr']),decreasing=T),lty=3,col='blue')
  #lines(sort(as.vector(times)),sort(predict.lm(Mast1C,interval='confidence')[,'upr'],decreasing=T),lty=3,col='blue')

  slopeGot=c(kM1L,kM1,kM1U)
  intGot=c(aM1L,aM1,aM1U)
  kGot=rbind(slopeGot,intGot);rownames(kGot)=c("Slope","Intercept")
  colnames(kGot)=c("reg 2.5%","reg mean k","reg 97.5%");kGot
  return(kGot)
}

#r1=regK(d,1)
#r2=regK(d,2)
#r3=regK(d,3)
#r4=regK(d,4)
#r=regK(d)
