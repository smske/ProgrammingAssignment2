## Function used to create matrix of object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  
	cacheInverse <- NULL
    set <- function(y) {
    	x <<- y
        cacheInverse <<- NULL }
    get <- function() x
    setInverse <- function(inverse) cacheInverse <<- inverse
    getInverse <- function() cacheInverse
    list(set = set,
    	 get = get,
         setInverse = setInverse,
         getInverse = getInverse)
          }
}

## Function uses makeCacheMatrix, where checks if inverse was calculated, then this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cacheInverse <- x$getInverse()
        if (!is.null(cacheInverse)) {
                message("computes cache")
                return(cacheInverse)
        }
        m <- x$get()
        cacheInverse <- solve(m, ...)
        x$setInverse(cacheInverse)
        cacheInverse
}

