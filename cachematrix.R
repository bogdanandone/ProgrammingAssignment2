## A more objectual implementation of the caching concept.
## This implementation hosts inverse calculation and caching management
## inside the 'makeCacheMatrix' function, packing data and behavior together

## The function returns a list of "methods" which can be applied
## - set: set a matrix
## - get: get the matrix
## - get.inverse: returns the inverse of the matrix from the cache; if the
##                cache is empty the function calculates the inverse and 
##                store the value in cache
## 
## Input parameters:
##  x: an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getinverse <- function() {
    if (is.null(i)) {
      if (is.null(x)) {
        warning("input matrix is NULL; inverse will be also NULL")
      }
      else {
        message("First time access inverse calculation")
        i <<- solve(x)
      }
    }
    else {
      message("Getting cached data")
    }
    i
  }
  list(set = set, get = get,
       get.inverse = getinverse)
  
}


## Just a simple wrapper for makeCacheMatrix$get.inverse() function
##
## Input parameters:
##  x: an instance of makeCacheMatrix function filled in with an invertible matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x$get.inverse()
}
