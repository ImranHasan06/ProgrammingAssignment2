## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly. 
## makeCacheMatrix and cacheSolve functions compute and cache inverse of given matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## This function can 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) matrix <<- inverse
  getInverseMatrix <- function() matrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve can retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invM <- x$getInverseMatrix()
  if(!is.null(invM)) {
    message("Getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setInverseMatrix(invM)
  invM
}
