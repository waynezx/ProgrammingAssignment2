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
	setinverse <- function (inverse) inver <<- inverse
	getinverse <- function () inver
	list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse)
}




## The cacheSolve funtion computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse hase been calculated
## (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inver <- x$getinverse()
	if(!is.null(inver)) {
			message("getting cached data")
			return (inver)
	}
	data <- x$get()
	inver <- solve(data,...)
	x$setinverse(inver)
	inver
}

