## These functions create the inverse of a matrix and cache it globally JPJ

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) minverse <<- inverse
  getInverse <- function() minverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function gives the inverse of the matrix from "makeCacheMatrix.R" 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cache solve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getInverse()
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setInverse(minverse)
  minverse
  
  }
