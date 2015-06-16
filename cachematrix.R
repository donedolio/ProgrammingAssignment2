## These 2 functions will cache the inverse of a matrix to minimize
## the use of resources when calculating the inverse multiple times


## Creates a list of functions to get/set value of matrix 
## and get/set inverse of matrix
makeCacheMatrix <- function(x = matrix()) {

    #Check if the function input is a matrix, if not return error message
    if(!is.matrix(x)) stop("x must be a matrix")
    
    #Declare the 4 functions to get/set value of matrix and inverse of matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Calculate and return the inverse of the matrix created on 
## the "makeCacheMatrix" function.
## Checking first if the inverse has already been calculated
cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    # Check if inverse of matrix has already been calculated
    # If yes, return cached inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If inverse hasn't been calculated, calculate and return inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  
}
