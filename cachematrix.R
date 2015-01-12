## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(data = matrix()) {
    cached = NULL
    set <- function (value) {
        data <<- value
        cached <<- NULL
    }
    
    get <- function () data
    
    setSolve <- function (value) cached <<- value
    getSolve <- function () cached
    
    list (set = set, get = get, getSolve = getSolve, setSolve = setSolve) 
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getSolve()
    if (is.null(inverse)){
        inverse <- solve(x$get(), ...)
        x$setSolve(inverse)
    } else {
        message("getting cached data")
    }
    
    inverse
}
