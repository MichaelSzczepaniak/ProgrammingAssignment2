## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function(x) {
        x
    }
    setInverse <- function(invers) {
        inv <<- invers
    }
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # inverse has already been calculate
        message("getting data from cache")
    }
    # inverse has been cached, so calculate it
    dataMatrix <- x$get()  # get the data out of special "matrix" object (list)
    inv <- solve(dataMatrix)
    x$setInverse(inv)  # update the special "matrix" object (SMO) so that the
                       # next time cacheSolve is called on the same SMO, the
                       # cached value will be returned instead of recalculating
    inv
}
