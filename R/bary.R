bary=function(v1.ba,v2.ba,v3.ba,x.ba,y.ba){
  #return bary centric coordinates
  one.ba=matrix(1,1,length(x.ba))
  A.ba=rbind(rep(1,3),c(v1.ba[1],v2.ba[1],v3.ba[1]),c(v1.ba[2],v2.ba[2],v3.ba[2]))
  lam.ba=solve(A.ba) %*% rbind(one.ba,t(x.ba),t(y.ba))
  return(t(as.matrix(lam.ba)))
}
