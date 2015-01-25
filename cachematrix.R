## System to cache matrix inverse calculations
## 


## This is a 'factory' function for a cached matrix inverse calculator

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv_in) inv <<- inv_in
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## This is the evaluation method for the cached matrix inverse calculator
## It will used cached values when available, otherwise re-cache and return
## newly calculated values

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

