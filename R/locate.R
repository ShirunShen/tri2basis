#################### locate the target row ###################
locate=function(matrix1.lc,matrix2.lc){
  colnames(matrix1.lc)=NULL
  colnames(matrix2.lc)=NULL
  n1.lc=nrow(matrix1.lc)
  n2.lc=nrow(matrix2.lc)
  ind.lc=rep(0,n1.lc)
  for(j in 1:n1.lc){
    for(k in 1:n2.lc){
      if(all(matrix1.lc[j,]==matrix2.lc[k,])) {ind.lc[j]=k;break}
    }
  }
  return(ind.lc)
}
