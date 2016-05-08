## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. These
## functions are used cache the inverse of a matrix.

## This function makes a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setInverse <- function(solve) inv <<- solve
 getInverse <- function() inv
 list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}
## This function calculates the matrix's inverse. It checks if
## the inverse has already been computed. If it does, the result comes up and and the computation does not. If the inverse has not been computed, 
## the computation of the inverse pops up, and the setinverse function gets the inverse into the cache.

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 inv <- x$getInverse()
 if (!is.null(inv)) {
   message("getting cached data")
   return(inv)
 }
 mat <- x$get()
 inv <- solve(mat, ...)
 x$setInverse(inv)
 inv
 }
