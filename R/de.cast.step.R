de.cast.step=function(lam1.bo,lam2.bo,lam3.bo,Bin.bo){
  #generate spline basis
  #lam1.bo=as.matrix(lam1.bo)
  #lam2.bo=as.matrix(lam2.bo)
  #lam3.bo=as.matrix(lam3.bo)
  Bin.bo=as.matrix(Bin.bo)
  m.bo=nrow(Bin.bo)
  d.bo=degree(m.bo)
  n.bo=length(lam1.bo)
  result=indices(d.bo)
  I.bo=result[,1];J.bo=result[,2];K.bo=result[,3]
  result=indices(d.bo-1)
  I1.bo=result[,1];J1.bo=result[,2];K1.bo=result[,3]
  index1.bo=locate(cbind(I1.bo+1,J1.bo,K1.bo),cbind(I.bo,J.bo,K.bo))
  index2.bo=locate(cbind(I1.bo,J1.bo+1,K1.bo),cbind(I.bo,J.bo,K.bo))
  index3.bo=locate(cbind(I1.bo,J1.bo,K1.bo+1),cbind(I.bo,J.bo,K.bo))
  if (ncol(Bin.bo)==1)
    Bout.bo=Bin.bo[index1.bo]%*%t(lam1.bo)+Bin.bo[index2.bo]%*%t(lam2.bo)+Bin.bo[index3.bo]%*%t(lam3.bo)
  else{
    if(length(lam1.bo)>1){
      Bout.bo=Bin.bo[index1.bo,]%*%diag(lam1.bo,length(lam1.bo),length(lam1.bo))+Bin.bo[index2.bo,]%*%diag(lam2.bo,length(lam2.bo),
                                                                                                           length(lam2.bo))+Bin.bo[index3.bo,]%*%diag(lam3.bo,length(lam3.bo),length(lam3.bo))}
    else{
      Bout.bo=Bin.bo[index1.bo,]*lam1.bo+Bin.bo[index2.bo,]*lam2.bo+Bin.bo[index3.bo,]*lam3.bo}
  }
  return(Bout.bo)
}
