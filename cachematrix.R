## These functions allow computation of the matrix inversion and store 
## the result in a cache, so the computation is not performed repeatedly
## for the same matrices.
##
## Usage:
## First, create the cached matrix object by calling:
##
## cacheMatrix <- makeCacheMatrix(x)
##
## where x is the matrix we want to be cached along with its inverse.
##
## Then, we can get the matrix inverse simply by calling:
##
## cacheSolve(cacheMatrix)
##
## When called for the first time, the matrix inverse will be computed.
## Every other consequent call will result into returning cached
## matrix inverse, without computing it again.
##


## This function will create a special object representing a cached
## matrix and its inverse. 
##
## Parameter x - the matrix of which the inverse has to be computed
##               and cached.
## Returns - cached matrix object

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix.
## If the matrix inverse has been already calculated, it returns
## the cached inverse matrix.
##
## Parameter x - cached matrix object returned by the function makeCacheMatrix
## Returns - matrix inverse, either from cache or calculated

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
