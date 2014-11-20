## The following two functions provide the functionality of creating a matrix object and caching its inverse matrix

## This function create a matrix object from the given input matrix, which can cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list( set = set, get = get, 
          setInverse = setInverse,
          getInverse = getInverse )
}


## This function is similar to the solve function, but will use the cached inverse matrix if available

cacheSolve <- function(x, ...) {
  
    inv <- x$getInverse()
    if( !is.null(inv) ) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
