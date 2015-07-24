## Matrix inversion is a costly computation.  This pair of functions caches the
## inverse of the matrix to avoid repeatedly computing it.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL
    set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(matrixinverse) inverseX <<- matrixinverse
    getmatrixinverse <- function() inverseX
        
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the
## inverse from the cache. If not, it computes the inverse and sets the
## value in the cache via setmatrixinverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrixinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, pwd...)
    x$setmatrixinverse(inv)
    inv
}
