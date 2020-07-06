## This program contains a pair of functions that cache the inverse of a matrix 

## The makeCacheMatrix function creates a special object (matrix) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function()inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special object (matrix) returned by the function makeCacheMatrix from above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
