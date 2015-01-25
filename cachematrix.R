## Put comments here that give an overall description of what your
## functions do

## Retrurn Matrix structure contains matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set and get matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x

    ## set and get matrix inverse
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    
    ## List of function
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Return inverse matrix either from cache or newly generated

cacheSolve <- function(x, ...) {
     ## Check if inverse has been generated
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv) ## inverse is generated, return cache inverse
    }
    
    ## No inverse available in cache, compute inverse 
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}
