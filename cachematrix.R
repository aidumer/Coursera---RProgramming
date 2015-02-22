## The following function stores a matrix and a cached value of
## the inverse of the matrix.

## At first, nothing is cached so "m" is set to NULL.

## The flow is to store a matrix, then return the stored matrix,
## then cache the inverse of the matrix, and then return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function actually performs the inverse of the matrix.
## First, the cached value is obtained. If it exists, the message below is returned.
## If it does not, the inverse of the matrix is calculated and then stored in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
