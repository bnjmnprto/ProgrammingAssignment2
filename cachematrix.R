## These functions store a matrix and cache its inverse to avoid
## recalculating the inverse when the matrix has not changed.

## Create a matrix object that can store and retrieve its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}

## Return the cached inverse or calculate and store it if needed.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
