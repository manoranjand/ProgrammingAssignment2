#Functions to Create inverse of a matrix and cache it

# makeCacheMatrix creates a list containing a function to (set/ get)
# (value/ value of inverse) of a matrix
makeCacheMatrix = function(x = matrix()) {
    i = NULL
    set = function(y) {
        x <<- y
        i <<- NULL
    }
    get = function() x
    setinverse = function(inverse) i <<- inverse
    getinverse = function() i
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve = function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i = x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

# How to use
# x = matrix(1:4,2,2)
# m = makeCacheMatrix(x)
# m$get()
# cacheSolve(m)
