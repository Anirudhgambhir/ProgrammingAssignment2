## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## make cache Matrix
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        # set function to set values
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        # to get value of matrix
        get <- function() x
        set_inv <- function(inverse) inv_m <<- inverse
        get_inv <- function() inv_m
        list(set = set, get = get,set_inv = set_inv,get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #to get inv if present already or not
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv_data <- x$get()
        #to compute inverse
        inv <- solve(inv_data)
        x$set_inv(inv)
        inv
}
