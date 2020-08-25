yadaline <- function(xvec,w,par){
  if(par==1){
      xvec<-cbind(1,xvec)}
  
  u<-xvec %*% w
  y<-u
  return ((as.matrix(y))) 
}