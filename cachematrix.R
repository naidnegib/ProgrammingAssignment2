## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). This code is for the assignment to 
## @write a pair of functions that cache the inverse of a matrix.

# Tip for testing:
#     m <- replicate(5, rnorm(5))   # Random 5x5 matrix
#     solve(m)                      # Inverse matrix of m

# Test code:
#  
#  A <- replicate(5000, rnorm(5000))
#  a <- makeCacheMatrix(A)
#  cacheSolve(a)
#  A <- replicate(5000, rnorm(5000))
#  cacheSolve(a) # It should be faster if properly cached
#  a <- makeCacheMatrix(A)
#  cacheSolve(a) # As a has changed it should be slow again
#  cacheSolve(a) # cached!



## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##    should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  
}
