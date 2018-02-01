

##Function creates matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


##Function computes the inverse of the matrix returned by  makeCacheMatrix as defined above 
##If the inverse has already been calculated (and matrix hasnt changed), then  cacheSolve  should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("Getting cached data...")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
