################## cr.indices
##################
cr.indices=function(d.ci,r.ci){
  I1.ci=integer(0)
  start.ci=1
  D.ci=d.ci+1
  for (j in 0:r.ci){
    for (k in 0:(r.ci-j)){
      new.col.ci=(start.ci+k):(start.ci+k+d.ci-r.ci)
      I1.ci=cbind(I1.ci,new.col.ci)
    }
    start.ci=start.ci+D.ci
    D.ci=D.ci-1
  }
  I2.ci=(-r.ci*r.ci/2+r.ci*(d.ci+3/2)+1):(-r.ci*r.ci/2+r.ci*(d.ci+1/2)+d.ci+1)
  return(list(as.matrix(I1.ci),as.matrix(I2.ci)))
}
