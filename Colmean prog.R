# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
#
# Notes:
# not sure how the "x = numeric()" part works in the argument list of the 
# function, but it seems to be creating a variable "x" that is not reachable 
# from the global environment, but is available in the environment of the 
# makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        inve <- NULL
                # store a matrix
        set <- function(y) {
                x << newValue
                # store a matrix
                inve <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inve <<- inverse
        getInverse <- function() inv
        # return a list. Each named element of the list is a function
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(x, ...) {
        # get the cached value
        inv <- x$getInverse()
        # if a cached value exists return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
         # return the inverse
        inv
}
