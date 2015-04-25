## Check if an inverted matrix was already calculated
## If positive, then return chached inverted matrix

## Calculates an inverted matrix and put it on cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    calcInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    ## List to be returned
    list(set = set, get = get,
    calcInverse = calcInverse,
    getInverse = getInverse)

}


## If an inverted matrix is already in cache then return cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
            message(getting cached data")
            return(m)
        }
        ## If no cache, need to calculate it and cache it
        data <- x$get()
        m <- solve(data, ...)
        x$calcInverse(m)
        m
}
