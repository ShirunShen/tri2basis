#' @title barycentric coordinates
#' @description obtain the barycentric coordinates
#' @param v1.ba the location of the first node in the target triangle
#' @param v2.ba the location of the second node in the target triangle
#' @param v3.ba the location of the third node in the target triangle
#' @param x.ba the x coordinate of the evaluated points
#' @param y.ba the y coordinate of the evaluated points
bary=function(v1.ba,v2.ba,v3.ba,x.ba,y.ba){
  #return bary centric coordinates
  one.ba=matrix(1,1,length(x.ba))
  A.ba=rbind(rep(1,3),c(v1.ba[1],v2.ba[1],v3.ba[1]),c(v1.ba[2],v2.ba[2],v3.ba[2]))
  lam.ba=solve(A.ba) %*% rbind(one.ba,t(x.ba),t(y.ba))
  return(t(as.matrix(lam.ba)))
}
