makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inve <<- inverse
        getInverse <- function() inve
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}