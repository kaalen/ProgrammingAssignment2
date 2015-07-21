## These two functions implement a complex data type that supports caching of 
## results for inverse matrix computation and the meth

## Creates a cached matrix object which supports retrieval of previously 
## calculated cached value
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) im <<- inverse_matrix
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function returnes cached inverse of a matrix or calculates the 
## inverse if it hasn't been calculated previously.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()          # Get the raw matrix from cached matrix object
    im <- solve(data, ...)   # Calculate inverse of matrix
    x$setinverse(im)         # Set the inverse of matrix so it is cached
    im
}
