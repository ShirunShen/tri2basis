#' @title local evaluation
#' @description used in the major beval function to obtain the local evaluation
#' @param lam1.le the first coefficient of barycentric polynomial
#' @param lam2.le the second coefficient of barycentric polynomial
#' @param lam3.le the third coefficient of bary centric polynomial
loceval=function(lam1.le,lam2.le,lam3.le,bcoef.le){
  nc.le=length(bcoef.le)
  d.le=degree(nc.le)
  #        browser()
  for (j in 1:d.le) bcoef.le=de.cast.step(lam1.le,lam2.le,lam3.le,bcoef.le)
  y=t(bcoef.le)
  return(y)
}
