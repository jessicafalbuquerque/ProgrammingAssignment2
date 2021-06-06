##  The following two functions manipulate the lexical scope of the r language 
## to preserve the state of an object in R in a different environment than the 
## one created and cache the result of an matrix operation.

## This function calculates the inverse of an matrix (x) and caches the result.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function recovers the matrix inverse calculation performed by the 
## previous function. If the result is not available in cache, the function 
## calculates the inverse and returns the result.

cacheSolve <- function(x, ...) {
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
 

