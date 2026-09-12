## These two functions work together to cache the inverse of a matrix,
## so that if the inverse has already been calculated, it can be
## retrieved from the cache instead of recomputed.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse. It returns a list of functions to set and get the
## value of the matrix, and to set and get the value of the inverse.

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

## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), it retrieves the inverse from
## the cache instead of recomputing it.

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
