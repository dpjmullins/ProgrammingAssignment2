## The following pair of functions makeCacheMatrix() and cacheSolve() cache the inverse of a matrix
## so that computing power and time can be saved, in the event of multiple iterations involving the
## same computation. 


## makeCacheMatrix() creates a special matrix, which includes a set of 4 functions for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    # invmat  acronym for inverted matrix
    invmat <- NULL
    # set() - function for changing the input matrix
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    # get() - function for retrieving the input matrix
    get <- function() x
    # setinvmat() - function for setting the inverted matrix
    setinvmat <- function(inv) invmat <<- inv
    # getinvmat() - function for retrieving the inverted matrix
    getinvmat <- function() invmat
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}


## cacheSolve() takes a special matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinvmat()
    # If statement to check if the inverted matrix has already been computed
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    # retrieves the stored matrix
    data <- x$get()
    # calculates the inverted matrix
    invmat <- solve(data, ...)
    # stored the inverted matrix in the cache
    x$setinvmat(invmat)
    invmat
}
