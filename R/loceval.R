loceval=function(lam1.le,lam2.le,lam3.le,bcoef.le){
  nc.le=length(bcoef.le)
  d.le=degree(nc.le)
  #        browser()
  for (j in 1:d.le) bcoef.le=de.cast.step(lam1.le,lam2.le,lam3.le,bcoef.le)
  y=t(bcoef.le)
  return(y)
}
