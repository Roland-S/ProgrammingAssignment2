
## This function takes a matrix as its argument and sets the inverse matrix to NULL.
## It then makes public a set of functions which can be used to determine if the inverse
## of the matrix was already calculated and cached.
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    ## This function sets a new matrix and NULLs the inverse matrix because
    ## the matrix and consequently its inverse were either created the first
    ## time or modified.
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    
    ## This function returns the matrix. This is called by cacheSolve
    ## in order to calculate the inverse of the matrix.
    get <- function() x
    
    ## This function sets the inverse. This is called by cacheSolve, in which
    ## the inverse was solved.
    setinverse <- function(inv) inverse <<- inv
    
    ## This function returns the inverse matrix. This is called by cacheSolve.
    getinverse <- function() inverse
    
    ## This list is a set of functions which are made public and can be used by other
    ## functions with the $ operator.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}




## This function gets the cached inverse matrix.

cacheSolve <- function(x, ...) {

    ## Here we get the inverse matrix using the $ operator to access getinverse
    ## created by makeCacheMatrix.
    inv <- x$getinverse()

    ## If the inverse matrix is not NULL, then return the inverse matirx.
    if(!is.null(inv)) 
    {
        message("Getting the cached matrix.")
        return(inv)
    }
    
    ## Here we get the matrix because the inverse above was NULL.
    data <- x$get()
    
    ## This function calculates the inverse function using the "solve" function.
    inv <- solve(data, ...)
    
    ## This function caches the inverse in x so it doesn't have to be recalculated.
    x$setinverse(inv)
    
    inv
}
