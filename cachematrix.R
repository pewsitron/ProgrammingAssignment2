## Provides functions for creating a cache matrix which
## is a matrix capable of caching its inverse so that
## it only needs to be computed once.

## Creates a cache matrix. Returns a list of functions
## which are used to access the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(get = get, set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Computes the inverse of a cache matrix or returns
## a cached inverse if it has been computed earlier.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}