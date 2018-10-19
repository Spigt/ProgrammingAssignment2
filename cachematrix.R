## Heavily inspired by the makeVector and cachemean functions
## in the example, the below functions allow caching the inverse
## of a matrix. Caching the inverse might save valuable
## computation time when the inverse of x would else be computed
## several times (e.g. in a loop).


## The function makeCacheMatrix creates an object containing a 
## matrix x and a reserved variable I for its inverse, together
## with functions for reading and writing the both.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve returns the inverse of 'x'. The
## inverse is either obtained from the cached data, or computed
## and subsequently cached if no cached inverse of 'x' is 
## available. The matrix 'x' is assumed to be invertible.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}
