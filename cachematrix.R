## Caching the inverse of a matrix


## Loading the matlib package

library(matlib)

## Function that creates a matrix object which can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  invr<- NULL
  set <-function(y) {
    x <<- y
    invr <<- NULL
  }
  get <-function() x
  setinv <- function(inv) invr <<- inv
  getinv <- function() invr
  list (set=set, get =get, setinv=setinv, getinv=getinv)
}


## Function which computes the inverse of the matrix created with the above function

cacheSolve <- function(x, ...) {
  invr <- x$getinv ()
  if(!is.null(invr)) {
    message ("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinv(invr)
  invr
}
