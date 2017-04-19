## makeCacheMatrix essentially adds support for caching an inverted matrix. 
## cacheSolve returns the inverse of a matrix, either from the cache or from
## calling solve(). 

## makeCacheMatrix provides support for caching the inverse of the supplied
## matrix through the use of the <<- operator, which provides the ability to
## maintain state across function invocations. The cached value is obtained
## via the getinverse function. If this function returns null, that is an
## indication that the inverse for a given matrix has not been cached. In this
## scenario, the setinverse function can be used to populate the cache with a
## computed inverse value. Subsequent calls to the getinverse function of the
## given matrix will returned the cached data.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes as a parameter a matrix created via the makeCacheMatrix
## function, which makes available the getinverse and setinverse functions.
## This function will check the cache for a cached inverse of the supplied
## matrix. If a cache hit occurs, then the cached value is returned, thus
## avoiding the overhead of computing the inverse of the matrix via the
## solve function. If a cache miss occurs, then the inverse matrix is computed
## via solve and this value is then cached, so that subsequent calls for the
## given matrix inverse will return the cached inverse value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i
}
