## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
	
	set <- function(y) {
		x <<- y
		inv_x <<- NULL
	}	
	get <- function() x
	setinv <- function(inverse_x) inv_x <<- inverse_x
	getinv <- function() inv_x
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv_x <- x$getinv()
	if (!is.null(inv_x)) {
		message("getting cached data")
		return(inv_x)
	}
	data <- x$get()
	inv_x <- solve(data, ...)
	x$setinv(inv_x)
	inv_x
}
