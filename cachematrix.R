
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # stores the inverse matrix
    inv <- NULL
    # sets the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # gets the matrix
    get <- function() x
    # sets the inverse matrix
    setinv <- function(inverse_matrix) inv <<- inverse_matrix
    # gets the inverse matrix
    getinv <- function() inv
    # returns a list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## checking if matrix inverse is cached
    if(!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    matrix <- x$get()
    # computing inverse
    inv <- solve(matrix)
    # caching inverse for future calls
    x$setinv(inv)
    inv
}
