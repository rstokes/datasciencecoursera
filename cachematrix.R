## Assignment 2: Caching compute intensive results using R scoping
## makeCacheMatrix
## cacheSolve

## Takes a matrix and returns a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) cachedInverse <<- inv
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Takes a special "matrix", calcuates and caches it's inverse.
## Subsequent calls will return the cached result. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}