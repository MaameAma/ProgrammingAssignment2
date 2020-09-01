## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##library(MASS)
library(MASS)

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL               ##Inverse set to null 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()(x)      ## function to get the matrix 
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}



## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve<-function(x,..){           ##cache data retrieved 
  inv<-x$getInverse()
  if(!is.null(inv)){                  ##Inverse check 
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv                               ##Calculate inverse value 
}
  

