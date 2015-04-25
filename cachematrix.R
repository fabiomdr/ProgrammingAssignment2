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
    inverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    inverse = inverse,
    getinverse = getinverse)

}


## If an inverted matrix is already in cache then return cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message(getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$inverse(m)
        m
}
