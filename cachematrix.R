## This functions cache the inverse of  given matrix in order to
## use it if the inverse is required several times instead of recomputing it

## this function makes list of four functions 
##  set -->set the matrix which inverse is required
## get--> get current matrix
## getInverse--> get inverse of current matrix , null otherwise
## setInverse  --> compute the inverse for the first time



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## return the cached inverse if exist , or compute it otherwise

cacheSolve <- function(x, ...) {
 m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
