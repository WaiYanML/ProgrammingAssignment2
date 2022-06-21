## Put comments here that give an overall description of what your
## functions do
## These functions are to return inverse of a matrix.

## Write a short comment describing this function
## This function is to create a special matrix that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv<<-inverse
  getinverse<- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##This function calculates the inverse of the matrix created by the above function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
