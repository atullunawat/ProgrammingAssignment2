# Cache the inverse of a matrix

# create the cache matrix object
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinvmat <- function(invmat) m <<- invmat
	getinvmat <- function() m
	list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}

# check the cache for existing inverse value before actually calculating one
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinvmat()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinvmat(m)
	m
}
