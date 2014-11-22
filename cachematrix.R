## This code takes advantage of the power of lexical scoping in R.
## The following functions will take a square matrix and produce the inverse matrix. This
## can be a computationally intensive operation. Storing a cached value of the inverse 
## matrix can reduce processor load and computation time. The functions below capture the 
## matrix object, check to see if the stored matrix value is NULL, and calculates the 
## inverse matrix if the stored value is NULL. If the stored matrix is already
## has been calculated, compute the inverse, store the result, and test the next matrix

## makeCacheMatrix creates a matrix object to be used later by cacheSolve

makeCacheMatrix <- function(x = matrix()) { # the input "x" will be a matrix
        m <- NULL             ## "m" is the inverse matrix, and will reset to NULL
                              ## with each call to makeCacheMatrix()
        set <- function(y) {  ## takes the input matrix
                x <<- y       ## saves it to "x"
                m <<- NULL    ## resets the inverse matrix to NULL
        }
        get <- function() {x} ## returns the cached value of the original matrix
        setinv <- function(inverse) {m <<- inverse} 
                              ## called by cacheSolve() during the first iteration; stores
                              ## the value of the inverse matrix
        getinv <- function() {m} ## this returns the cached value of the inverse matrix to
                                 ##cacheSolve() on subsequent calls
        list(set = set, get = get, ## "list()" is accessed each time makeCacheMatrix() is
             setinv = setinv,      ## called
             getinv = getinv)
}


cacheSolve <- function(x, ...) {  ## x is an object created by makeCacheMatrix
        m <- x$getinv()           ## accesses x and gets the value of the inverse matrix
        if(!is.null(m)) {         ## if the inverse matrix is not NULL,
                message("getting cached data")  ## print this message to the console,
                return(m)         ## and return the inverse matrix, then end
        }
        data <- x$get()       ## conditional path if x$getinv() is NULL
        m <- solve(data, ...) ## calculate the inverse of matrix
        x$setinv(m)           ## store the inverse matrix in x
        m        ## Return the inverse
}
