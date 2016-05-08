## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    y <- NULL
    setMatrix <- function(z) {
        x <<- z
        y <<- NULL
    }
    getMatrix <- function() {
        x
    }
    setInverseMatrix <- function(z) {
        y <<- z
    }
    getInverseMatrix <- function() {
        y
    }
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    y <- x$getInverseMatrix()
    if(!is.null(y)) {
        message("getting cached data")
        return(y)
    }
    z <- x$getMatrix()
    y <- solve(z)
    x$setInverseMatrix(y)
    y
}

## test with a matrix
## A <- matrix(c(1,3,3,1), nrow = 2, ncol = 2)
## B <- makeCacheMatrix(A)
## cacheSolve(B)
## cacheSolve(B)
