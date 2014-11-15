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

## Output Sample:

# > x <- matrix(1:4, 2, 2)
# > m <- makeCacheMatrix(x)
# > m$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# Getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
