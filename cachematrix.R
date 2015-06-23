## This function takes a matrix x as input and constructs an object which holds
## x, its inverse inv, and 4 functions: two that get and set x and two that get
## and set inv.
##
## This object is used in conjunction with the cacheSolve function in order to
## cache the calculation of inv so that unnecessary/repeated computations of
## the same inverse can be avoided.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # stays NULL until the first time setinv is called
    # loads the matrix y into the parent environement
    set <- function(y) {
        x <<- y
        inv <- NULL  # clears the prior computed inverse to reflect
                     # initial state of new matrix
    }
    # gets the matrix the return object (list) wraps
    get <- function() {
        x
    }
    # sets the computed inverse matrix (invers) in the parent environment
    setinv <- function(invers) {
        inv <<- invers
    }
    # gets the inverse matrix if it was set (by setinv) or returns NULL
    # if it was not
    getinv <- function() {
        inv
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes an input x that is a matrix wrapper object created from
## a call to makeCacheMatrix and returns the inverse of the matrix which the x
## object wraps.
##
## Additional paremeters may be passed to this function, but they are optional.
## Because this function uses the R solve(a, b, ...) function to compute the
## inverse, any parameters passed besides x are assumed to be intended for the
## solve(a, b, ...) function.  If no additional parameters besides x are passed,
## default values will be used in the call to solve.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        # inverse has already been calculate
        message("getting data from cache")
    }
    # inverse has been cached, so calculate it
    dataMatrix <- x$get()  # get the data out of special "matrix" object (list)
    inv <- solve(dataMatrix, ...)
    x$setinv(inv)  # update the special "matrix" object (SMO) so that the
                   # next time cacheSolve is called on the same SMO, the
                   # cached value will be returned instead of recalculating
    inv
}
