makeCacheMatrix <- function(x = matrix()) {
	## Creates a list of functions used to cache/store
	## a matrix and its inverse
	## Usage example:
	##	m <- makeCacheMatrix(matrix(1:4, 2, 2))
	##	...
	##	m$set(matrix(c(1, -1, 0, 1), 2, 2))


	# Inverse matrix is initiated with NULL value
        inverse <- NULL

	# Set/get functions
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) inverse <<- solved
        getinverse <- function() inverse

	# Return the functions as a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
	## Calculate the inverse of a matrix, using makeCacheMatrix
	## to "cache" the inverse.  That is, if the inverse had
	## been calculated previously, then get the saved inverse
	## and return it.
	##
	## Usage example:
	##	m <- makeCacheMatrix(...)
	##	cacheSolve(m)


	# Check if there is a previously calculated inverse
	# If there is, then return the inverse (and exit function)
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached inverse matrix")
                return(inverse)
        }

	# Calculate and store inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
