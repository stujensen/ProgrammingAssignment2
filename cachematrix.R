## StuJensen R code for Assignment 2 07/01/2017
## This function should create a special matrix object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## sets the inverse value (invMatrix) to NULL
    invMatrix <- NULL
    ## a set function to assign x and invMatrix variables in parent env
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    ## a get function to get x from the parent environment
    get <- function() x
    ## a set-inverse function to set inv function to invMatrix
    setInv <- function(Inv) invMatrix <<- Inv
    ## a get-inverse function that returns invMatrix variable
    getInv <- function() invMatrix
    ## create a named list of the above functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}

## Write a short comment describing this function
## This function computes the inverse of the special matrix
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the
## cache. 
cacheSolve <- function(x, ...) {
    ## getinv function to retrieve the value of invMatrix to invMatrix
    invMatrix <- x$getInv()
    ## check if invMatrix is not null - if so retrieve the cache value
    if(!is.null(invMatrix)) {
        message("Getting cached value...")
        return(invMatrix)
    }
    ## otherwise, calculate the inverse
    ## use get to retrieve x and solve it
    d <- x$get()
    invMatrix <- solve(d)
    ## use the set function to set invMatrix
    x$setInv(invMatrix)
    ## return the matrix value
    return(invMatrix)
    
}
