## These functions speed up to inverse a bunch of matrices

## This function returns a list of functions to get and set a matrix and inverse of it.

makeCacheMatrix <- function(x = matrix()) {

    inver <- NULL

    set <- function(y) {                                   # Sets the matrix
    
        x <<- y
        inver <<- NULL
    }
    
    get <- function() x                                   # Gets the matrix

    
    setinv <- function(inverse) inver <<- inverse         # Sets the inverse matrix
    
    
    getinv <- function() inver                            # Gets the inverse matrix

   
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function first check whether the inverse matrix calculated or not. 
## If not it calculate it and save in cache and returns it.

cacheSolve <- function(x, ...) {
        
    inver <- x$getinv()

    
    if (!is.null(inver)) {                               # If the inverse of the matrix is already calculated, returns it
        message("getting cached data")
        return(inver)
    }

    
    data <- x$get()                                      # If the inverse matrix has not yet calculated, calculate it
    inver <- solve(data, ...)

    
    x$setinv(inver)                                      # Cache the inverse matrix


    inver                                                # Return it

}
