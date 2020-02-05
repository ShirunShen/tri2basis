######################## locEng
########################
locEng=function(v1.lo,v2.lo,v3.lo,Mat.lo,d.lo,index.lo){
  D.lo=(d.lo+1)*(d.lo+2)/2
  Id.lo=diag(rep(1,D.lo))
  vx.lo=c(1,0)
  vy.lo=c(0,1)
  result=tcord(v1.lo,v2.lo,v3.lo,vx.lo)       #tcord
  lam1x.lo=result[,1];lam2x.lo=result[,2];lam3x.lo=result[,3]
  result=tcord(v1.lo,v2.lo,v3.lo,vy.lo)
  lam1y.lo=result[,1];lam2y.lo=result[,2];lam3y.lo=result[,3]
  Dx.lo=dirder(Id.lo,lam1x.lo,lam2x.lo,lam3x.lo)
  Dxx.lo=dirder(Dx.lo,lam1x.lo,lam2x.lo,lam3x.lo)
  Dxy.lo=dirder(Dx.lo,lam1y.lo,lam2y.lo,lam3y.lo)
  Dy.lo=dirder(Id.lo,lam1y.lo,lam2y.lo,lam3y.lo)
  Dyy.lo=dirder(Dy.lo,lam1y.lo,lam2y.lo,lam3y.lo)
  #        browser()
  if(index.lo==1){
    K3.lo=abs(triarea(v1.lo,v2.lo,v3.lo))*(t(Dxx.lo)%*%Mat.lo%*%Dxx.lo+2*t(Dxy.lo)%*%Mat.lo%*%Dxy.lo+t(Dyy.lo)%*% Mat.lo%*%Dyy.lo)
  }
  if(index.lo==2){
    K3.lo=abs(triarea(v1.lo,v2.lo,v3.lo))*(t(Dxx.lo+Dyy.lo)%*%Mat.lo%*%(Dxx.lo+Dyy.lo))
  }
  return(K3.lo)
}
