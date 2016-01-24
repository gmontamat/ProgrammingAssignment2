## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. The following two functions are used to
## cache the inverse of a matrix.


## makeCacheMatrix defines a special kind of matrix that allows the
## inverse of the matrix to be cached.

makeCacheMatrix <- function(x = matrix()) {
    ## Returns a list containing the following functions:
    ## set: sets the value of a matrix
    ## get: returns the value of the stored matrix
    ## setinv: sets the value of the inverse
    ## getinv: returns the value of the stored inverse
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result
## and skips the computation. If not, it computes the inverse and
## stores it for future calls.

cacheSolve <- function(x, ...) {
    ## Assumes that the matrix is always invertible
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
