## The functions take a matrix to solve for its inverse and set the value in the cache,
## so when we need the inverse, it will look it up first in the cache to save time(if the
## inverse already exists). 

## makeCacheMatrix creates a list of functions to set/get the matrix,
## and set/ get the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- Null
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks to see if the inverse has already been calculated. If so, it skips
## the computation and gets the inverse from the cache. Otherwise, it solves for the inverse
## and sets the value in the cache as well.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
        ## Return a matrix that is the inverse of 'x'
}
