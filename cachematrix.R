## The following two functions can cache and compute the 
## inverse of a matrix.


## This function creates a special MATRIX object
## that can cache its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtrx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special
## MATRIX which is returned by the above function. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.


cacheSolve <- function(mtrx, ...) {
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtrx$get()
  invserse <- solve(data, ...)
  mtrx$setinv(inverse)
  return(inverse)
}
