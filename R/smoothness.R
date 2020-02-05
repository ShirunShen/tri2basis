#' @title smoothness function
#' @description create the smoothness matrix
#' @param V.sm vectices coordinates
#' @param T.sm indices of the vectices of each triangle
#' @param d.sm degree of barycentric polynomial
#' @param r.sm degree of derivative smoothness condition
#' @return smoothness matrix
#' @export
smoothness=function(V.sm,T.sm,d.sm,r.sm){
  #define smoothness matrix H
  result=tdata(V.sm,T.sm)  ##tdata:store triangle in 4 different way
  E.sm=result[[1]];TE.sm=result[[2]];TV.sm=result[[3]];EV.sm=result[[4]]
  int.sm=which(apply(TE.sm,2,sum)>1) #interior edge indicator
  n.sm=length(int.sm)                #number of interior edges
  N.sm=0          #number of constraint point of each interior edge
  Neq.sm=0        #number of smoothness constraints on each interior edge
  result=crarrays(d.sm,r.sm)   #need to read again
  I1.sm=result[[1]]
  I2.sm=result[[2]]
  I.sm=list();J.sm=list();K.sm=list()
  for(j in 0:r.sm){
    N.sm=N.sm+((j+1)*(j+2)/2+1)*(d.sm+1-j)
    Neq.sm=Neq.sm+d.sm+1-j
    result=indices(j)
    LI.sm=result[,1]
    LJ.sm=result[,2]
    LK.sm=result[,3]
    I.sm[[j+1]]=LI.sm
    J.sm[[j+1]]=LJ.sm
    K.sm[[j+1]]=LK.sm
  }
  nbasis.sm=(d.sm+1)*(d.sm+2)/2
  Index1.sm=matrix(0,N.sm*n.sm,1)
  Index2.sm=matrix(0,N.sm*n.sm,1)
  values.sm=Index1.sm
  A.sm=matrix(c(1,2,2,3,3,1,2,1,3,2,1,3),ncol=2,byrow=T)

  #        browser()

  for (j in 1:n.sm){
    k.sm=int.sm[j]                 #int.sm:indice of interior edges
    AdjT.sm=which(TE.sm[,k.sm]!=0) #triangles with edge k.sm

    v1.sm=E.sm[k.sm,1]             #vertex 1 of the edge k.sm
    t1.sm=AdjT.sm[1]               #triangle 1 adjacent to edge k.sm
    T1.sm=T.sm[AdjT.sm[1],]        #vertexes of the triangle 1 ad...
    v2.sm=E.sm[k.sm,2]             #vertex 2 of the edge k.sm
    t2.sm=AdjT.sm[2]               #triangle 2 adjacent to edge k.sm
    T2.sm=T.sm[AdjT.sm[2],]        #vertexes of the triangle 2 ad...

    i1.sm=which(T1.sm==v1.sm)      #indice of vertex of triangle 1 that start the edge we consider now
    j1.sm=which(T2.sm==v1.sm)      #indice of vertex of triangle 2 that.
    i2.sm=which(T1.sm==v2.sm)      #indice of vertex of triangle 1 that end the edge we consider now
    j2.sm=which(T2.sm==v2.sm)      #indice of vertex of triangle 2 that.

    e1.sm = locate(matrix(c(i1.sm,i2.sm),nrow=1),A.sm)
    e2.sm = locate(matrix(c(j1.sm,j2.sm),nrow=1),A.sm)

    if (e1.sm >3) {
      e1.sm=e1.sm-3
      temp.sm=T1.sm
      T1.sm=T2.sm
      T2.sm=temp.sm
      temp.sm=e1.sm
      e1.sm=e2.sm
      e2.sm=temp.sm
      temp.sm=t1.sm
      t1.sm=t2.sm
      t2.sm=temp.sm
    } else
      e2.sm=e2.sm-3

    v4.sm=T2.sm[T2.sm%in%c(v1.sm,v2.sm)==FALSE]
    V4.sm=V.sm[v4.sm,]             #coordinate of v4.sm
    result=bary(V.sm[T1.sm[1],],V.sm[T1.sm[2],],V.sm[T1.sm[3],],V4.sm[1],V4.sm[2])
    lam1.sm=result[,1]
    lam2.sm=result[,2]
    lam3.sm=result[,3]
    lambda.sm=c(lam1.sm,lam2.sm,lam3.sm)

    if(e1.sm==2){
      temp.sm=lambda.sm[1]
      lambda.sm[1]=lambda.sm[2]
      lambda.sm[2]=lambda.sm[3]
      lambda.sm[3]=temp.sm
    }
    if(e1.sm==3){
      temp.sm=lambda.sm[2]
      lambda.sm[2]=lambda.sm[3]
      lambda.sm[3]=temp.sm
    }
    VarCT.sm=0
    EqCt.sm=0
    for (k in 0:r.sm){
      lam.sm=factorial(k)/(gamma(I.sm[[k+1]]+1)*gamma(J.sm[[k+1]]+1)*gamma(K.sm[[k+1]]+1))*lambda.sm[1]^I.sm[[k+1]]*lambda.sm[2]^J.sm[[k+1]]*lambda.sm[3]^K.sm[[k+1]]
      T1mat.sm=as.matrix(I1.sm[[k+1]][[e1.sm]])
      T2vector.sm=I2.sm[[k+1]][[e2.sm]]
      numeq.sm=nrow(T1mat.sm)
      numVar.sm=(ncol(T1mat.sm)+1)*numeq.sm
      T1Values.sm=matrix(1,nrow(T1mat.sm),ncol(T1mat.sm))%*%diag(lam.sm)
      eqnums.sm=(j-1)*(Neq.sm)+EqCt.sm+diag(1:numeq.sm)%*%matrix(1,numeq.sm,ncol(T1mat.sm)+1)
      Index1.sm[((j-1)*N.sm+VarCT.sm+1):((j-1)*N.sm+VarCT.sm+numVar.sm)]=eqnums.sm
      Index2.sm[((j-1)*N.sm+VarCT.sm+1):((j-1)*N.sm+VarCT.sm+numVar.sm)]=c((t1.sm-1)*nbasis.sm+T1mat.sm,(t2.sm-1)*nbasis.sm+T2vector.sm)
      values.sm[((j-1)*N.sm+VarCT.sm+1):((j-1)*N.sm+VarCT.sm+numVar.sm)]=c(T1Values.sm,-1*rep(1,length(T2vector.sm)))
      VarCT.sm=VarCT.sm+numVar.sm
      EqCt.sm=EqCt.sm+numeq.sm
    }
  }
  H.sm=matrix(0,n.sm*Neq.sm,nrow(T.sm)*nbasis.sm)
  H.sm[cbind(Index1.sm,Index2.sm)]=values.sm
  return(H.sm)
}
