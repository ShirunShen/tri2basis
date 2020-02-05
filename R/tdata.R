################## tdata
################## return the vertex, edge and etc
tdata=function(v.t,t.t){
  #v.t: vertices coordinates ((x,y))
  #t.t: triangle vertices indices (counter clockwise)
  ntri.t=nrow(t.t) #number of triangles
  #initial empty space
  Edges.t=matrix(rep(integer(0),4),ncol=2) #edge's end points indices in V
  TE.t=rep(integer(0),ntri.t) #indicate triangle's edges; row: triangle; col: edge
  numEdges.t=0

  for (j in 1:ntri.t){
    Tj.t=t.t[j,]
    for (k in 1:3){
      edge.t=c(min(Tj.t[k],Tj.t[k%%3+1]),max(Tj.t[k],Tj.t[k%%3 + 1])) #edge.t=sort(c(Tj.t[k],Tj.t[(k+1)%%3])) #ordered vertices for edge.t
      if (nrow(Edges.t)>0 )
      {edgenum.t=which((edge.t[1]==Edges.t[,1])*(edge.t[2]==Edges.t[,2])==1)}
      else
      {edgenum.t=integer(0)}

      if (length(edgenum.t)==0) {
        Edges.t=rbind(Edges.t,edge.t)
        numEdges.t=numEdges.t+1
        TE.t=cbind(TE.t, rep(0,ntri.t)) #start from a boundary edge
        edgenum.t=numEdges.t
      }
      TE.t[j,edgenum.t]=1
    }#end k loop
  }#end j loop

  numV.t=dim(v.t)[1]
  TV.t=matrix(0,ntri.t,numV.t) #indicate triangle vertices; row: triangle;col: vertices
  for (j in 1:ntri.t){
    TV.t[j,t.t[j,]]=1
  }

  EV.t=matrix(0,numEdges.t,numV.t) #indicate edge vertices -- row: edge; col: vertixes
  for (j in 1:numEdges.t){
    EV.t[j,Edges.t[j,]]=1
  }
  #bdr.t=findbdt(t.t,v.t,edge.t,te.t,ev.t)

  return(list(Edges.t,TE.t,TV.t,EV.t))
}
