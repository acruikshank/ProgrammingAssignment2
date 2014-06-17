## makeCacheMatrix and cacheSolve allow you to calculate the inverse
## of a matrix and cache the result

## Create a data structure containing a matrix and, optionally, its inverse
## and return getters and setters for both.
makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinverse <<- inverse
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Given a CacheMatrix return or compute and cache the inverse of the matrix.
cacheSolve <- function(x) {
        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        matrix <- x$get()
        xinverse <- solve(matrix)
        x$setinverse(xinverse)
        xinverse
}
