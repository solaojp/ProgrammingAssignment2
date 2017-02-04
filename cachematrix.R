## Put comments here that give an overall description of what your program
## FUNCTION CACHES INVERSE OF MATRIX

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) 
    i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Computes inverse of special "Matrix"

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    i
  }

