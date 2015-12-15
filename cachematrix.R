## Matrix inversion is usually a costly computation therfore we provide a 
## caching option using a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## The returned "object" (it's really a list) has 4 functions:
## 1. set -         set the value of the matrix
## 2. get -         get the value of the matrix
## 3. setinverse -  set the value of the inverse matrix
## 4. getinverse -  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversed <<- inverse
  getinverse <- function() inversed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getinverse()
  if(!is.null(inversed)) {
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinverse(inversed)
  inversed
}
