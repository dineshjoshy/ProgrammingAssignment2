# A pair of functions that cache the inverse of a matrix
# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initializes the value of inverse matrix 
  
  invs <- NULL
  
  # set the value of the matrix
  
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  
  # get the value of the matrix
  
  get <- function() {
    x
  }
  
  # set the value of the inverse matrix
  
  setInverse <- function(invInput) {
    invs <<- invInput
  }
  # get the value of the inverse matrix
  
  getInverse <- function() {
    invs
  }
  
  # returns a list of all the above functions
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## The following function calculates the inverse of the special 
## "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached,
  # if so, we get the inverse from the cache directly
  
  invs <- x$getInverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  
  # else, we first get the matrix
  
  dat <- x$get()
  
  #  Now calculate the inverse
  
  invs <- solve(data, ...)
  
  # next, cache the inverse of the matrix
  
  x$setInverse(invs)
  
  # and finally, return the result
  
  invs
}