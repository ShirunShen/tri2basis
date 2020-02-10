#' @title bivariate spline basis functions
#' @description create bivariate spline basis functions on triangulation
#' @param v.se the \eqn{N\times 2} vertices matrix. Each row represents the location of
#' one vertex
#' @param t.se the \eqn{n\times 3} triangle indices. Each row represents the number of
#' three vertices that forms a triangle. The order of vertices should be
#' counterclockwise
#' @param d.se degree of polynomial functions
#' @param x.se the x coordinate of evaluated points for basis functions
#' @param y.se the y coordinate of evaluated points for basis functions
#' @export
beval=function(v.se,t.se,d.se,x.se,y.se){
  #create b-spline basis
  #v.se: vertices
  #t.se: edges
  #d.se: bspline degree
  #x.se, y.se: (x,y) coordinates
  tol=100*.Machine$double.eps
  ntri.se=nrow(t.se)
  nbasis.se=(d.se+1)*(d.se+2)/2
  ind.se=rep(1,length(x.se))
  bmatrix.se=matrix(0,length(x.se),nbasis.se*nrow(t.se))
  #        browser()
  for (i in 1:ntri.se){
    #barycentric coordinates of (x.se, y.se) within triangle i
    result=bary(v.se[t.se[i,1],],v.se[t.se[i,2],],v.se[t.se[i,3],],x.se,y.se)
    lam1.se=result[,1];lam2.se=result[,2];lam3.se=result[,3];
    I.se=((lam1.se>=-tol) *(lam2.se>=-tol) * (lam3.se>=-tol)==1)
    I.se=which(I.se&ind.se) #only points within triangle i and no bspline coordinates
    if(length(I.se)>0){ #(x.se,y.se) have >0 barycentric coordinates
      ind.se[I.se]=0#to avoid double count on-triangle-edge points
      for(j in 1:nbasis.se){
        c.se=rep(0,nbasis.se)
        c.se[j]=1
        bmatrix.se[I.se,(i-1)*nbasis.se+j]=loceval(lam1.se[I.se],lam2.se[I.se],lam3.se[I.se],c.se)
      }
    }
  }
  return(bmatrix.se)
}
