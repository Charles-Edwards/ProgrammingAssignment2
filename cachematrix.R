## These functions calculate the inverse of a matrix and then cache the calculation so that it can later be accessed 
##if the same calculation is repeated 

## This function creates a special vector which is really a list containing a function to set and get 
##the values of the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {


m <- NULL
y <- NULL
setmatrix <- function(y) { 
x <<- y      
m <<- NULL
}
getmatrix <- function() x
setinverse <- function(mean) m <<- mean
getinverse <- function() m
list(setmatrix = setmatrix, getmatrix = getmatrix,
setinverse = setinverse,
getinverse = getinverse)
}

## This function calculates the inverse of the special vector created above, but first checks to see if the inverse has
##already been calculated and can be retrieved from the cache instead 

cacheSolve <- function(x, ...) {
        
 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m}
