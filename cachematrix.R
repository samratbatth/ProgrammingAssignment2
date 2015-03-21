## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix This function creates a datastructure that caches the results 
##                  by using lexical scoping.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve   This function returns an inverse of a matrix.
##              The results are cached in the makeCacheMatrix datastructure 
##              to further optimize the runtime.
## input        a makecacheMatrix datastructure
## output       returns an inverse of the matrix
##
cacheSolve <- function(x, ...) {
  
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
