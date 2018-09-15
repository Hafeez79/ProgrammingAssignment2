## The first function makeCacheMatrix will create an object that can cache cache inverse of a matrix. 
## the second function cacheSolve returns inverse of the matrix.

## This function builds a list of functions and data objects. This list is made available in the environment
## of makeCacheMatrix so that its elements can be used by the following function cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) mx <<- solve
  get_inv <- function() mx
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## The function checks if data is already available in mx. If so, it'll return the data. If not, it will
## compute the inverse, cache (i.e. store) the value in mx and return the value also.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mx <- x$get_inv()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$set_inv(mx)
  mx
}
