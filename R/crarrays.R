################## crarrays##################
crarrays=function(d.cr,r.cr){
  I1.cr=list();I2.cr=list()
  for (j in  0:r.cr){
    result=crcellarrays(d.cr,j)
    I1.cr[[j+1]]=result[[1]]
    I2.cr[[j+1]]=result[[2]]
  }
  return(list(I1.cr,I2.cr))
}
