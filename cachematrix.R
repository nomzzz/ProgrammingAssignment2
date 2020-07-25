## create a special "matrix" object that can cache its inverse
## then compute the inverse of the special "matrix" 
## or retrieve the inverse from the cache if it has already been calculated


## This function creates a special "matrix" object that can cache its ##inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    ##set matrix x to the matrix y in cached environment
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##create a function that gets matrix x
    get <- function() x
    ##in cached environment, set i to the value of the inverse of matrix
    setSolve <- function(Solve) i <<- Solve
    ##from cached envorinment, get the inverse, i, of the matrix
    getSolve <- function() i
    ##make list of all the functions
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by ## makeCacheMatrix above or retrieve the inverse from the cache if it has ## already been calculated
cacheSolve <- function(x, ...) {
	##assign i to the inverse of the matrix if it has already been calculated
    i <- x$getSolve()
    ##if inverse has been calculated, return inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ##if inverse has not been calculated, get matrix and calculate inverse
    data <- x$get()
    i <- solve(data, ...)
    ##set the value of the inverse of the matrix in the cached environment
    x$setSolve(i)
    ## Return a matrix that is the inverse of 'x'
    i       
}
