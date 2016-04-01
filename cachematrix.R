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
