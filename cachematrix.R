## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a list containing a function to
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse (setInverse)
## get the value of the inverse (getInverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function returns the inverse of a matrix. It first searched if it is already cached and
## if not, calculates it and caches it for future reference.

cacheSolve <- function(x, ...) {

  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("Returning cached data:")
  } else {
    message("Cached matrix inverse not found. Inverse calculated:")
    originalMatrix <- x$get()
    matrixInverse <- solve(originalMatrix)
    x$setInverse(matrixInverse)
  }
  matrixInverse
}
