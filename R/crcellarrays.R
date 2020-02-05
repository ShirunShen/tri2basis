##################crcellarrays
##################
crcellarrays=function(d.cs,r.cs){
  I1.cs=list();I2.cs=list();
  result=cr.indices(d.cs,r.cs)
  J1.cs=result[[1]];J2.cs=result[[2]]
  D1.cs=rep(0,d.cs+1)
  D2.cs=rep(0,d.cs+1)
  s1.cs=d.cs+1
  s2.cs=1
  for (j in 1:(d.cs+1)){
    D1.cs[j]=s1.cs
    s1.cs=s1.cs+d.cs+1-j
    D2.cs[j]=s2.cs
    s2.cs=s2.cs+d.cs+2-j
  }
  I2.cs[[1]]=flip(J2.cs)
  Temp.cs=D1.cs-r.cs
  I2.cs[[2]]=flip(Temp.cs[1:(d.cs+1-r.cs)])
  Temp.cs=D2.cs+r.cs
  I2.cs[[3]]=as.matrix(Temp.cs[1:(d.cs+1-r.cs)])
  I1.cs[[1]]=J1.cs
  Temp.cs=matrix(0,nrow(J1.cs),ncol(J1.cs))
  for (j in 0:r.cs){
    Temp.cs[,j+1]=D1.cs[(j+1):(d.cs+1-r.cs+j)]
  }
  loc.cs=r.cs+2
  back.cs=r.cs+1
  if(r.cs>0){
    for (j in 1:r.cs){
      for(k in 0:(r.cs-j)){
        Temp.cs[,loc.cs]=Temp.cs[,loc.cs-back.cs]-1
        loc.cs=loc.cs+1
      }
      back.cs=back.cs-1
    }
  }
  I1.cs[[2]]=Temp.cs
  Temp.cs=matrix(0,nrow(J1.cs),ncol(J1.cs))
  for(j in 0:r.cs){
    Temp.cs[,j+1]=D2.cs[(j+1):(d.cs+1-r.cs+j)]
  }
  loc.cs=r.cs+2
  back.cs=r.cs+1
  if(r.cs>0){
    for (j in 1:r.cs){
      for(k in 0:(r.cs-j)){
        Temp.cs[,loc.cs]=Temp.cs[,loc.cs-back.cs]+1
        loc.cs=loc.cs+1
      }
      back.cs=back.cs-1
    }}
  I1.cs[[3]]=flip(Temp.cs)
  return(list(I1.cs,I2.cs))
}
