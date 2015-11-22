## The two functions cache the inverse of a matrix so that it can be
## looked up in the cache rather than recomputed when needed.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function (y) {
		x<<-y
		inver <<- NULL
	}
	get <- function() x
	setInverse <- function (inverse) inver <<- inverse
	getInverse <- function () inver
	list(set=set, get=get,
		setInverse = setInverse,
		getInverse = getInverse)
}




## The cacheSolve funtion computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse hase been calculated
## (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inver <- x$getInverse()
	if(!is.null(inver)) {
			message("getting cached data")
			return (inver)
	}
	data <- x$get()
	inver <- solve(data,...)
	x$setInverse(inver)
	inver
}

