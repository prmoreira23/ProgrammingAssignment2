## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is a function that creates a special object that
## stores a matrix and allows you to take action on the matrix.
## cacheSolve is a function that will allow you to calculate the inverse
## of a matrix from makeCacheMatrix,
## pulling the inverse from cache if it was already calculated previously.
  
## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" that consists of functions to
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse
makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
        
        setMatrix <- function(newMatrix) {
            x <<- newMatrix
            cache <<- NULL
        }

        getMatrix <- function() {
            x
        }
        
        cacheInverse <- function(solve) {
            cache <<- solve
        }

        getInverse <- function() {
            cache
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve calculates the inverse of the special matrix created in makeCacheMatrix
## however, it also first checks to see if the inverse was already calculated and if it
## has been, grabs the value from cache instead of computing it again
cacheSolve <- function(y, ...) {
        inverse <- y$getInverse()
       
        if(!is.null(inverse)) {
            message("Getting cached data")
            return(inverse)
        }
        
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)

        inverse
}
