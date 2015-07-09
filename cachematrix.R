## Code stores the results of an inversed matrix and 
## returns the results of a previous calcuation if already done.

## Stores the matrix and the inversed matrix for future use.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the submitted matrix or returns 
## the previous value if matrix is the same as before.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <-x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
