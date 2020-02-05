################## triarea ##################
triarea=function(v1.ta,v2.ta,v3.ta){
  #calculate area of a triangle defined by the 3 vertices
  x.ta=v1.ta[1];y.ta=v1.ta[2]
  a.ta=v2.ta[1];b.ta=v2.ta[2]
  c.ta=v3.ta[1];d.ta=v3.ta[2]
  A.ta=(a.ta-x.ta)*(d.ta-y.ta)-(c.ta-x.ta)*(b.ta-y.ta)
  A.ta=A.ta/2
  return(A.ta)
}
