## Put comments here that give an overall description of what your
## functions do

## This function creates a new object which has 4 "methods"
## set, get, setinverse and getinverse
## This object also has 1 "member" called "inverse" used
## to cache its value
## The set method reinitialize the cached inverse
## setinverse and getinverse operate over the cached value

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse<<- NULL
  }
  get <- function() x
  setinverse <- function(m) inverse <<- m
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function returns the inverse of an
## CacheMatrix object

cacheSolve <- function(x, ...) {
  ## Get the old value
  m <- x$getinverse()
  ## If the old value isn't null return it
  ## and print a message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Any other case, recalculate its value
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
