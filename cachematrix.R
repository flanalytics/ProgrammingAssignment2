## Put comments here that give an overall description of what your
## functions do

## This function is used to convert a matrix "x" into a format suitable for feeding to 
## the cacheMatrix function to find the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function is used to calculate the inverse of a matrix
## If it has already calculated the inverse for this matrix the cached value is returned
## If there is no cached result it uses the R solve() function to calculate 
## the inverse and caches the result for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  



}
