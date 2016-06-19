## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix that can cache its inverse by the given matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
	set <- function(y) {
		x <<- y
		inv_x <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv_x <<- inverse
	getinverse <- function() inv_x
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_x <- x$getinverse()
	if (!is.null(inv_x)){
		message("getting cached inverse matrix")
		return(inv_x)
	}
	inv_x <- solve(x$get())
	x$setinverse(inv_x)
	inv_x
}
