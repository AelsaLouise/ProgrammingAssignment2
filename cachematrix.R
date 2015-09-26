## Store a matrix, and allow its inverse to be cached, so it can be accessed 
## without needing to be recalculated every time.
## Assume the matrix is invertible

## Create a matrix with an extra variable to hold its inverse, 
## and four accessor functions to get and set these values.
## The inverse is not calculated in this function.

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    get <- function() { m }
    setInverse <- function(newinverse) { inverse <<- newinverse }
    getInverse <- function() { inverse }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## For makeCacheMatrix object x, check whether the inverse has already been
## calculated. If it has, return that. Otherwise, calculate the inverse,
## cache it for future use, and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
