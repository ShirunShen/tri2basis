#' @title indices function
#' @description generate the (i,j,k) indices matrix such that i+j+k=d
indices=function(d.in){
  #generate indices i,j,k: i+j+k=d.in
  m.in=(d.in+1)*(d.in+2)/2
  I.in=rep(0,m.in)
  J.in=I.in
  K.in=I.in
  Mark.in=1
  for (j in seq(d.in,0,-1)){
    I.in[Mark.in:(Mark.in+j)]=seq(j,0,-1)
    J.in[Mark.in:(Mark.in+j)]=0:j
    K.in[Mark.in:(Mark.in+j)]=(d.in-j)*rep(1,j+1)
    Mark.in=Mark.in+j+1
  }
  return(cbind(I.in,J.in,K.in))
}
