####################### dirder
#######################
dirder=function(Bc.di,lam1.di,lam2.di,lam3.di){
  m.di=dim(Bc.di)[1]
  d.di=degree(m.di)
  DerBc.di=d.di*de.cast.step(lam1.di,lam2.di,lam3.di,Bc.di)
  return(DerBc.di)
}
