## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Following functions are used cache the inverse of a matrix.


## This function makes a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
 set <- function(y) {
   x <<- y
   inverse <<- NULL
 }
 get <- function() x
 setInverse <- function(solve) inverse <<- solve
 getInverse <- function() inverse
 list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}
## The following function calculates the matrix's inverse. 
## It checks if the inverse has already been computed. 
## If it has already been done the result comes up. If the inverse has not been calculated yet, 
## the computation of the inverse is shown, and the setinverse function gets the inverse into the cache.

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 inverse <- x$getInverse()
 if (!is.null(inverse)) {
   message("getting cached data")
   return(inverse)
 }
 matrix <- x$get()
 inverse <- solve(matrix, ...)
 x$setInverse(inverse)
 inverse
 }

