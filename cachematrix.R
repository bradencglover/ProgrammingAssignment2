## These functions create matrix, solve for its inverse, and
## cache that inverse to save compuatation time in the future.

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## Computes the inverse of the matrix created by makeCacheMatrix or returns cached inverse if it exists

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)){
    message("Getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Test of the functions
x <- matrix(data = 1:4, nrow = 2, ncol = 2)
f <- makeCacheMatrix(x)
cacheSolve(f)
