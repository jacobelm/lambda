Lambda<-function(X,k){
  
  if (k==1){
    x=as.matrix(X[1,])
    grandsum=as.numeric(t(x)%*%x)
  }  
  else if(k>=1){
    X=as.matrix(X[1:k,]) #first k rows of X
    
    XT=t(X)
    
    A=as.matrix(X%*%XT) #need the grandsum of this matrix
    
    e1=as.matrix(rep(1,k)) #create vector of k 1's
    
    grandsum=as.numeric(t(e1)%*%A%*%e1) #formula for grandsum
  }
  return(grandsum)
}










