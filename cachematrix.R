##Two functions to interact with matrices, and calculate and cache the matrix inverse

## returns a matrix "object" that can cache its inverse, so it only needs calculated once
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  setInverse <- function(i) inverse <<- i 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix object applied -- calculates it if it hasn't been cached yet
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (is.null(inverse)) {
    newlyCalculatedInverse = solve(x$get(), ...)
    x$setInverse(newlyCalculatedInverse)
    newlyCalculatedInverse
  } else {
    message("using cached inverse")
    inverse
  }
}
