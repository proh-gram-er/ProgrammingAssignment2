## This set of functions provide a "cheaper" method of repeatedly retrieving
## the inverse of a matrix.
## 
## NOTE: The "x" parameter provided to makeCacheMatrix needs to be an
## invertible matrix, otherwise cacheSolve will throw an error

## This function takes an invertible matrix as an argument, and retunrns
## a special "matrix" object with getter and setter accessors for both the 
## matrix itself and the "cached" inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) {
    stop("x is not a matrix")
  }
  
  inv <- NULL
  get <- function()
    x
  set <- function(x) {
    x <<- x
    inv <<- NULL
  }
  
  getInverse <- function()
    inv
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function takes a "special" matrix object (returned by the 
## makeCacheMatrix function) and tries to calculate its inverse efficiently
## (checks to see if the value exists is cache, otherwise solve it and store
## it in cache and returns it)
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("inverse found in cache")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
