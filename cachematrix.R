## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
    
    # sets inverse matrix to NULL
    i <- NULL
    
    # defines functions for a final list
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    seti <- function(inverse) i <<- inverse
    geti <- function() i
    
    # defines and returns the list with all needed functions/methods
    list(set = set, get = get,
         seti = seti,
         geti = geti)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    # checks whether inverse matrix is in cache, if so - immediately returns it
    i <- x$geti()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # otherwise - calculate and return it
    data <- x$get()    
    i <- solve(data, ...)
    x$seti(i)
    i
}
