######################## build
######################## evaluate the matrix for inner product
build=function(d.bu){
  result=indices(d.bu)
  I.bu=result[,1];J.bu=result[,2];K.bu=result[,3]
  m.bu=(d.bu+1)*(d.bu+2)/2
  Mat.bu=matrix(0,m.bu,m.bu)
  for (j in 1:m.bu){
    for(k in 1:m.bu){
      Mat.bu[k,j]=choose(I.bu[j]+I.bu[k],I.bu[j])*choose(J.bu[j]+J.bu[k],J.bu[j])*choose(K.bu[j]+K.bu[k],K.bu[j])
    }
  }
  Mat.bu=Mat.bu/(choose(d.bu*2,d.bu)*choose(2*d.bu+2,2))
  return(Mat.bu)
}
