## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). This code is for the assignment to 
## @write a pair of functions that cache the inverse of a matrix.

# Tip for testing:
#     m <- replicate(5,norm(5))     # Random 5x5 matrix
#     solve(m)                      # Inverse matrix of m

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Sample code from the assignment (using a mean value)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
  
  ##### My code follows... (I hope) ####
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##    should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Sample code from the assignment (using a mean value)
  
  cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
  }
  
  ##### My code follows... (I hope) ####
  
  
}
