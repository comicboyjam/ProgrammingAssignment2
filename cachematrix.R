

## makeCacheMatrix creates a special "matrix" composed of a list which sets and gets the value of the matrix and sets and gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix created with makeCacheMatrix. First it checks to see if the inverse has already been calculated. If it has, it gets the inverse from the cache. If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache through the setinverse funtion

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
