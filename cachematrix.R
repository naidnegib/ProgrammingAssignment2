## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). This code is for the assignment to 
## @write a pair of functions that cache the inverse of a matrix.

## Tip for testing (create a matrix and compute its inverse):
##     m <- replicate(5, rnorm(5))   # Random 5x5 matrix
##     solve(m)                      # Inverse matrix of m

## TEST CODE
##  
##  A <- replicate(5000, rnorm(5000))   # Create a random matrix
##  a <- makeCacheMatrix(A)             # Create a "CacheMatrix" object from previous matrix
##  cacheSolve(a)                       # Test inverse. As it is new it must be calculated
##  A <- replicate(5000, rnorm(5000))   # New values to the original matrix (A)
##  cacheSolve(a)                       # As "CacheMatrix" special object is not modified 
##                                      # it should be quick to show the inverse of "Old-A"
##  a <- makeCacheMatrix(A)             # New "A" is now set to "a", so the cache should be deleted
##  cacheSolve(a)                       # As a has changed it should be slow again (cache cleaned in previous step)
##  cacheSolve(a)                       # cached!



## makeCacheMatrix: creates a special "matrix" object for "x" that can cache its inverse.

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
