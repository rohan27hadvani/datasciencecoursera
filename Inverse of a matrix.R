
makeCacheMatrix <- function(x = matrix()) {
    ## x must be an invertible matrix
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinverse = function(inverse) m <<- inverse
    getinverse = function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    ## x is the output of makeCacheMatrix()
    
    m = x$getinverse()
    
    if (!is.null(m)){
        message("getting cached data")
        print("data available in the cache:")
        return(m)
    }
    
    # otherwise, calculates the inverse
    print("data no available in the cache, so it's been calculated:")
    data = x$get()
    m = solve(data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(m)
    m
    
}
