## Put comments here that give an overall description of what your
## functions do

## This is the function for getting matrix inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
  get <- function() x
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  setInverse <- function(inverse) inverse <<- inverse
  
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## this will check for the cached valued, if its nonexisting it will solve the inverse



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
