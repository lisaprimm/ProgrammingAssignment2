##
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.  The assignment is to write a pair of functions that will
## cache the inverse of a matrix.
##
## makeCacheMatrix: This function creates a special 'matrix' object that
## can cache its inverse.
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
##
## cacheSolve: This function computes the inverse of the special 'matrix'
## returned by makeCacheMatrix above.  If the inverse was previously
## calculated, then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if (!is.null(m)) {
            message("Getting the Cached Inverse")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
