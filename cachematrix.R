## The functions makeCacheMatrix and cacheSolve allow you to cache the results of solving a matrix
## so when you have the same problem again, you can take advantage of the cache and avoid
## the cost of recalculating the inverse againl.

## This function creates a "matrix" you can 'set' and 'get'. It also allows you to 'set' and 'get'
## the cached inverse matrix (with 'setinverse', 'getinverse').
## A feature is that when you 'set' the matrix, you automatically invalidate the cache of the inverse

makeCacheMatrix <- function(x = matrix()) {
	## Initialize the cache
	cached_inverse <- NULL
	
	## When you set a matrix, invalidate the cache
	set <- function(m) {
		x <<- y
		cached_inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse_matrix) cached_inverse <<- inverse_matrix
	getinverse <- function() cached_inverse
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## This function applies the Solve function to the matrix only if it isn't cached already.
## After it has solved the matrix, it calls 'setinverse' to store and cache the result for future use.

cacheSolve <- function(x, ...) {
	
    ## First, try to retrieve the inverse matrix from the cache
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
