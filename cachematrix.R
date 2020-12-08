## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setImatrix <- function(Imatrix) m <<- Imatrix
        getImatrix <- function() m
        list(set = set, get = get,
             setImatrix = setImatrix,
             getImatrix = getImatrix)
}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getImatrix()
        if(!is.na(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setImatrix(m)
        m
}
