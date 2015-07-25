## These functions allow the creation of an object 'makeCacheMatrix' that 
## provides the user with the functionality of caching/storing the 
## inverse of this matrix.  This saves on repeated processing time given 
## very large matrices.  

## Note: this assumes that the matrix supplied for 'x' is always invertible.

## Write a short comment describing this function

## This function creates a 'CacheMatrix' object that stores a matrix and
## once accessed, its inverse, 'caching' the inverse by calling a second
## function, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
	
}


## This function actually creates the 'cache' for the matrix referenced in
## makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
