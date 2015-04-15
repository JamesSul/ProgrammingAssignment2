## The functions below enable the caching matrix inversions
## to provide potential speed improvements when repeatedly
## accessing those inversions. This enables the second and
## subsequent uses of the inversion to be a lookup from cache
## instead of recomputing the inversion.

## makeCacheMatrix can store and return a matrix object
## and its inverse using the set, setinverse, get, and
## getinverse methods

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y, r, c) {
                x <<- matrix(y, r, c)
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix.
## If the inverse was already created by
## makeCacheMatrix.setinverse, that value is returned
## along with the message "getting cached data"
## otherwise the inverse is calculated then returned
## with no message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
