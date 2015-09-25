## The makeCacheMatrix and cacheSolve function are used to save/retrieve the stored 
## matrix and calculate the inverse of the stored matrix respectively.

## The makeCacheMatrix function is responsible for storing the inverse of the
## matrix and returning the stored value.  It is also used to get and set the 
## value of the source matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(cache) m <<- cache
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## the cacheSolve function returns the inverse of a matrix, either by calculation
## or returning a value that was previously stored

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}
