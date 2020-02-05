######################## tcord
######################## return the barycentric coord of direction
tcord=function(v1.tc,v2.tc,v3.tc,v.tc){
  result1=bary(v1.tc,v2.tc,v3.tc,v.tc[1],v.tc[2])
  result2=bary(v1.tc,v2.tc,v3.tc,0,0)
  return(result1-result2)
}
