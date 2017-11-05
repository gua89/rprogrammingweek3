
## Put comments here that give an overall description of what your
## functions do


## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse (the solve function)
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        # defining function 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the invrese has already been calculated. 
## If so, it gets the invrese from the cache and skips the computation. 
## Otherwise, it calculates the invrese of the data and sets the value of the invrese in the cache via the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}