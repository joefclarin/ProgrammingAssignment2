## this function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  ## this initializes the inverse variable
  inv <- NULL
  
  ## this sets the matrix
  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  
  ## this gets the matrix
  get <- function() x
  
  ## this sets the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## this gets the inverse of the matrix
  getInverse <- function() inv
  
  ## this returns list of methods
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
  
}


## this function computes the inverse of the 
##    special "matrix" returned by makeCacheMatrix.
## if the inverse has already been calculated (and 
##    the matrix has not changed), then the cachesolve
##    should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  ## this returns a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## this return the inverse if it's already set
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  ## this gets the matrix from the x
  data <- x$get()
  
  ## this calculates the inverse using matrix multiplication
  inv <- solve(data)
  
  ## this sets the inverse to the object
  x$setInverse(inv)
  
  ## this returns the matrix
  inv
  
}
