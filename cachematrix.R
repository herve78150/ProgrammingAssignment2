## cacheSolve and cacheSolve work together. 
## They are used to chache a matrix and its inversse matrix. In this way we don't have 
## to calculate the inverse each time we need it. 

## makeCacheMatrix store a Matrix and its Inverse  and return a list of functions
## which allow to manipuate them.
## cacheSolve use an instance of makeCacheMatrix in order to produce the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() { inverse}
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSove calculates the inverse of a matrix using solve function and store it 
## using an instance of  makeCacheMatrix
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
