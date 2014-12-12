## the two function are working together

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        ## below functions are not ran but as tributes of the object
        ## return the original x
        get <- function() {x}
        ## store the inversion
        setiv <- function(inversion) {iv <<- inversion}
        ## show the inversion 
        getiv <- function() {iv}
        list(set = set, get = get,
             setiv = setiv,
             getiv = getiv)
      
             
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## x is an object
        iv <- x$getiv()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        # this is call the tribute of the x object created above
        iv <- solve(data, ...)
        x$setiv(iv)
        iv
}
