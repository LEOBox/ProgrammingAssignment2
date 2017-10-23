## These two functions are used to make a special matrix which can cache the inverse of
## matrix instead of computing matrix again and again

## makeCacheMatrix will create a special matrix object

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setmatrix <- function(solve) m <<- solve

	getmatrix <- function() m

	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## cacheSolve will inverse the matrix if the matrix is not calulated
## if the matrix is calulated, cacheSolve will find the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()

        if (!is.null(m)) {
        	message("GET CACHED DATA")
        	return (m)
        }

        matrix <- x$get()

        m <- solve(matrix, ...)
        x$setmatrix(m)

        m
}
