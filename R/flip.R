################## flip  ##################
flip=function(matrix.fl){
  #flip.mat=matrix.f1[nrow(matrix.f1):1,]
  matrix.fl=as.matrix(matrix.fl)
  n.fl=nrow(matrix.fl)
  m.fl=matrix(0,n.fl,n.fl)
  for(j in 1:n.fl){
    m.fl[j,n.fl-j+1]=1}
  return(m.fl%*%matrix.fl)
}                           #turn the matrix upside down
