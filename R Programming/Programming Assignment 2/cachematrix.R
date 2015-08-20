## The purpose of the following two function is to cache the computed inverse 
## of some matrix 'x' so that we do not have to repeatedly compute it. This 
## allows us to save computational resources.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	setmatrix <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}

	getmatrix <- function() x
	setinvmatrix <- function(inverse) invMatrix <<- inverse
	getinvmatrix <- function() invMatrix
	list(	setmatrix = setmatrix,
		getmatrix = getmatrix,
		setinvmatrix = setinvmatrix,
		getinvmatrix = getinvmatrix
	);
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmatrix <- x$getinvmatrix()
	if(is.null(invmatrix)) {
		m <- x$get()
		invmatrix <- solve(m)
		x$setinvmatrix(invmatrix)
		return(invmatrix)
	}
	x$getinvmatrix()
}
