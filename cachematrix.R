## Routines to support caching of matrix inversion.

## Description: This function creates a wrapper object around a matrix. The
##              wrapper object caches the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {

        ## Inverse is initially empty
        ##
        i <- NULL

        ## Routine to set the matrix. Invalidates the cached content.
        ##
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        ## Routine to recover the matrix
        ##
        get <- function() x

        ## Routine to cache the inverse
        ##
        setinv <- function(inv) i <<- inv

        ## Routine to recover the cached inverse
        ##
        getinv <- function() i

        ## Return a list with the routines for setting/getting content and
        ## cached matrix inverse.
        ##
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Description: This function returns the inverse of a cache matrix. If the
##              inverse has not been calculated, it will calculate it and store
##              it in the cache. Otherwise it simply returns the cached content.
##
cacheSolve <- function(x, ...) {

        ## Recover the cached content.
        ##
        i <- x$getinv()

        ## If we do not have a cached copy, calculate the inverse now.
        ##
        if (is.null(i)) {
                message("Creating cached inverse")

                ## Recover matrix
                m <- x$get()

                ## Get the inverse, using the caller supplied options.
                i <- solve(m, ...)

                ## Cache the inverst
                x$setinv(i)

        } else {
                message("Returning cached copy")
        }

        i
}
