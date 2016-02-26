## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates inverse of matrix.
## If inverse has already been calculated (and matrix hasn't changed)
## the cached result is printed instead of calculating again.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
