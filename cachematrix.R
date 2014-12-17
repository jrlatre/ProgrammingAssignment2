## Together, makeCacheMatrix and cachesolve offer a caching mechanism to manage a matrix and its inverse in such a way that whenever we need the inverse of the matrix we don't need to compute it unless a) we have never computed it yet or b) the cached matrix has changed.

## makeCacheMatrix creates four functions to manage a cache for a matrix and for the computation of its inverse. The cache is in the environment of the makeCacheMatrix function.
## cachesolve returns the cached inverse of the matrix we cached with makeCacheMatrix if this inverse already exists (i.e., if we have called cachesolve in the past and the matrix has not changed since). If there is no inverse cached, cachesolve computes it and caches it.
## To use these functions we have to assign the list of functions returned by makeCacheMatrix to a variable, say c. Then we can call get, set, getsolve, and setsolve using c$get, c$set, c$getsolve, and c$setsolve, which access the cached values of the matrix and its inverse inside the environment of makeCacheMatrix.

## For example:
## a <- matrix(c(3, 6, 7, 9),he matrix nrow=2, ncol=2)
## c <- makeCacheMatrix(a)
## ...use c$get() to get the cached matrix, c$set(<new_matrix>) to update the matrix.
## ...c$getsolve() to get the inverse of the cached matrix - or NULL, if the inverse needs to be computed.


## makeCacheMatrix creates and returns a list of four functions to update a cached matrix and its inverse, and to read them.
## set caches a matrix and "erases" the cached inverse of the previous cached matrix. We use it when the original matrix changes.
## get returns the cached matrix.
## getsolve returns the cached inverse.
## setsolve caches the inverse (to be used by cachesolve only - if you call this function elsewhere you will corrupt the cache).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y     ## Set the cached matrix to a new value.
        m <<- NULL  ## This is key: if the matrix changes (because we changed it by calling set) this piece of code sets the inverse to NULL, forcing a recomputation of the inverse when we call cachesolve.
    }
    get <- function() x  ## because x is bound to the matrix we want to cache, the cached matrix is bound to the get function when the function is created. get will always return the cached matrix.
    setsolve <- function(solve) m <<- solve  ## setsolve writes a new (inverse) matrix to the cache. In order for the caching system to work, only cachesolve can call setsolve.
    getsolve <- function() m    ## this function returns the inverse of the matrix or NULL. It returns NULL if the inverse has not been previously computed, or if the set function was called.
    list(set = set, get = get, ## this returns the functions in a list with tags to make them callable in the global environment.
    setsolve = setsolve,
    getsolve = getsolve)
}


## The cachesolve function returns the inverse of the cached matrix if the inverse has already been computed; and computes it if the cached value of the inverse is NULL.

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix cached in the environment of x.
    m <- x$getsolve()    ## x$getsolve and the other x$function functions exist if makeCacheMatrix was called and bound to a variable.
    if(!is.null(m)) {    ## m won't be null if a) an inverse was computed and stored into the cache environment previously, and b) if it was not wiped out when changing the value of the matrix with x$set.
        message("getting cached data")  ## let the user know there was a stored value for the inverse already.
        return(m)        ## there was a value in m; this returns the cached value and skips the code below.
    }
    data <- x$get()      ## the value of m was NULL; no inverse was stored. This assigns the cached matrix to the variable data.
    m <- solve(data, ...)  ## this computes the inverse of data; it also accepts other arguments to the solve function.
    x$setsolve(m)        ## this caches the new inverse.
    m                    ## return the new inverse.
}