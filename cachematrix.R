## Pair of functions to calculate inverse of matrix and use a cached inverse if calculated earlier

## Function: makeCacheMatrix
## This function calculates the inverse of a matrix and puts it into cache
## It also has option of set and get matrix value to and from cache for comparison

##ginv function from library MASS has been used here instead of solve()
##as it allows for calculation of inverse of non-square matrices as well

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
                	x <<- y
	                inv <<- NULL
	}
	get <- function() x
	setinv <- function(ginv) inv <<- ginv	## can be checked using inv %*% matX (should return identity matrix)
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
					## returning list of functions for the matrix
}

## Function: cacheSolve
## This function will check if the inverse of the matrix already exists in cache
## If yes, cached value is return, else calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached value of inverse:")
                return(inv)
        }
        data <- x$get()
        inv <- ginv(data, ...)
        x$setinv(inv)
        inv
}