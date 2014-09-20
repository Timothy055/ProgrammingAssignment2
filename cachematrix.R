# The following functions provide a memoized/cached version of matrix inversion.
# This helps save on computations by saving previously calculated inverses of a matrix
# if it has not changed.
# These functions trade memory for speed by saving the inverted matrices in a hidden
# vector in the cacheMatrix created by makeCacheMatrix.

# This function returns a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) xInv <<- solve
  getInverse <- function() xInv
  list(set = set,
       get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


# This functions calculates the inverse of a cache matrix. If the inverse has 
# previously been calulated it will return the inverse immediately without
# caching it. This can be used as a performance optimization when the inverse may
# need to be calculated many times.
# See test_cache_matrix.R for a simple benchmark program of calculating a cacheMatrix
# inverse repeatedly versus a normal matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if (!is.null(xInv)) {
    message("Getting cached inverse..")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInverse(xInv)
  xInv
}
