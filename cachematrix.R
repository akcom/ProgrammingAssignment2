## This file is used for caching matrix inversion, which is typically an expensive operation
## Basically what we're implementing is cached lazy evaluation of inversion

#create the "special" matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrx) {
    x <<- matrx
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matrx.inv) inv <<- matrx.inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function solves the matrix inversion if it has not been done already
## OR pulls the inversion from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (is.null(inv)) {
    inv <- solve(x$get())
    x$setinv(inv)
  }
  return(inv)
}

#test it
#cache <- makeCacheMatrix(matrix(runif(25), 5, 5))
#cacheSolve(cache)
