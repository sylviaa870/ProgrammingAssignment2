## the function show the steps how to cache and compute the inverse of a square matrix.
## the function of makeCacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the function help to compute the inverse of the special "matrix"
## It is returned by the above makeCacheMatrix function.
## If the inverse has been calculated earlier (and the matrix has not changed), 
## then this cacheSolve function the computation will be skipped
## and gets the inverse from the cache,
## else it will compute the inverse and set the inverse in the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
