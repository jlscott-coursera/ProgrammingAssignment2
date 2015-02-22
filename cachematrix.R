## A cacheMatrix is an object which stores a matrix and its inverse.
## "makeCacheMatrix" is used to create the object.
## "cacheResolve" is used to compute the inverse. Once computed, the 
## inverse is cached for future use.
##
## The cacheMatrix is a list containing the following functions:
##     "set" - for setting the matrix
##     "get" - for getting the stored matrix
##     "set.inverse" - for setting the inverse
##     "get.inverse" - for getting the stored matrix




## Creates a cacheMatrix.  x is the matrix to store.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    set.inverse <- function(i) inverse <<- i
    
    get.inverse <- function() inverse
    
    list(
        set = set,
        get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse
    )
}


## Return a matrix that is the inverse of 'x',
## computing it if necessary.
## 'x' is a cacheMatrix. 

cacheSolve <- function(x, ...) {

    inverse <- x$get.inverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    
    matrix <- x$get()
    
    inverse <- solve(matrix, ...)
    x$set.inverse(inverse)
    
    inverse
}
