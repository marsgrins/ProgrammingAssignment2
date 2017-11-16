## Put comments here that give an overall description of what your
## functions do

## Create a special matrix object that caches its inverse:

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates mean of special matrix, unless that has already been calculated
## and cached, in which case it returns the cached value.

cacheSolve <- function(x, ...) {
  m <-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
