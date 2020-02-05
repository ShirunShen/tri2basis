#' @title the area of the domain
#' @description calculate the area of the domain using Monte Carlo method
#' @param fsb the list of the boundary points of the domain, where the
#' @param n.area the number of Monte Carlo samples
#' @importFrom mgcv inSide
#' @export
emp.area=function(fsb,n.area){
  lowlim=100*.Machine$double.eps #define this in the function
  set.seed(20130425) #use fixed seed

  fsb.x.lb=min(fsb[[1]]$x)-lowlim
  fsb.x.ub=max(fsb[[1]]$x)+lowlim
  fsb.y.lb=min(fsb[[1]]$y)-lowlim
  fsb.y.ub=max(fsb[[1]]$y)+lowlim
  x=runif(n.area,fsb.x.lb,fsb.x.ub)
  y=runif(n.area,fsb.y.lb,fsb.y.ub)
  return(sum(inSide(fsb,x,y))/n.area*((fsb.x.ub-fsb.x.lb)*(fsb.y.ub-fsb.y.lb)))
}
