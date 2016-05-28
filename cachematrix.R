# Data Science (Johns Hopkins University)

# R Programming

# Programming Assignment 2

# Assignment: Programming Assignment 2: Lexical Scoping

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
# The assignment is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix is the function which creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  y <- NULL
  
  # set the matrix
  setMatrix <- function(z) {
    x <<- z
    y <<- NULL
  }
  # get the matrix
  getMatrix <- function() {
    x
  }
  # set the inverse of the matrix
  setInverseMatrix <- function(z) {
    y <<- z
  }
  # get the inverse of the matrix
  getInverseMatrix <- function() {
    y
  }
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


# cacheSolve is the function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  
  y <- x$getInverseMatrix()
  
  # check whether the inverse has already been calculated (and the matrix has not changed)
  if(!is.null(y)) {
    # retrieve the inverse from the cache
    message("getting cached data")
    return(y)
  }
  else {
    # calculate the inverse of the 'new' matrix
    z <- x$getMatrix()
    y <- solve(z)
    x$setInverseMatrix(y)
    return(y)
  }
}

# test with a matrix
# A <- matrix(c(1,3,3,1), nrow = 2, ncol = 2)
# B <- makeCacheMatrix(A)
# cacheSolve(B)
# cacheSolve(B)
