##this function creates a special "matrix" object that can cache its inverse.
makecachematrix<-function(x=matrix()){
  inverse_matrix<-NULL
  set<-function(y){
    x<<-y
    inverse_matrix<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)
    inverse_matrix<<-inverse
  getinverse<-function()inverse_matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
##this function compute the inverse of the special matrix called by the makecachematrix above. if the inverse has already been calculated then cachesolve should retrieve the inverse from the cache.
cachesolve<-function(x,...){
  ##return matrix that is inverse of 'x'
  inverse_matrix<-x&getinverse()
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(inverse_matrix)
  }
  data<-x&get()
  inverse_matrix<-solve(data,...)
  x&setinverse(inverse_matrix)
  inverse_matrix
}
