degree=function(m.dg){
  #calculate degree of bsplines
  #m.dg: number of basis (m=(d+1)(d+2)/2)
  d.dg=(-3+sqrt(8*m.dg+1))/2
  return(d.dg)
}
