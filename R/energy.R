###################### energy
###################### evaluate energy matrix

#' @title energy matrix
#' @description create the penalty matrix with respect to bivariate spline basis functions
#' @param V.e vectices coordinates
#' @param T.e indices of the vectices of each triangle
#' @param d.e degree of barycentric polynomial
#' @param index.e =1, if the intergral term exists interaction of two coordinates; =2, otherwise;
#'  default = 1
#' @return penalty matrix with respect to the bivariate spline basis functions
#' @export
energy=function(V.e,T.e,d.e,index.e){
  #penalty matrix
  ntri.e=nrow(T.e)
  D.e=(d.e+1)*(d.e+2)/2    #number of i+j+k = d
  Dsq.e=D.e^2
  Mat.e=build(d.e-2)       #build
  Index1.e=rep(0,ntri.e*Dsq.e)
  Index2.e=Index1.e
  S.e=Index1.e
  place.e=1
  #        browser()
  for (k in 1:ntri.e){
    LocK.e=locEng(V.e[T.e[k,1],],V.e[T.e[k,2],],V.e[T.e[k,3],],Mat.e,d.e,index.e)
    result=which(LocK.e!=0,arr.in=TRUE)
    i.e=result[,1];j.e=result[,2];s.e=LocK.e[cbind(i.e,j.e)]
    L.e=length(i.e)
    Index1.e[place.e:(place.e+L.e-1)]=(k-1)*D.e+i.e
    Index2.e[place.e:(place.e+L.e-1)]=(k-1)*D.e+j.e
    S.e[place.e:(place.e+L.e-1)]=s.e
    place.e=place.e+L.e
  }
  K.e=matrix(0,ntri.e*D.e,ntri.e*D.e)
  K.e[cbind(Index1.e[1:(place.e-1)],Index2.e[1:(place.e-1)])]=S.e[1:(place.e-1)]
  return(K.e)
}
