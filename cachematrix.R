## Put comments here that give an overall description of what your
## functions do
## test commit
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	matrixInverse <- NULL
	
	set <- function(y) {
		x <<- y
		matrixInverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverse) matrixInverse <<- inverse
	
	getInverse <- function() matrixInverse
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	matrixInverse <- x$getInverse()
	
	if (!is.null(matrixInverse)) {
		message("Retrieved cached data")
		return(matrixInverse)
	}        
	
	matrixData <- x$get()
	matrixInverse <- solve(matrixData, ...)
	x$setInverse(matrixInverse)
	matrixInverse
}
