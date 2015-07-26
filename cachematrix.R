## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create a special "CacheMatrix" which define function to set/get the value of the matrix and set/get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	# set the matrix
	set <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}
	# get the matrix
	get <- function() x
	# set the inverse of the matrix
	setInverseMatrix <- function(mat) invMatrix <<- mat
	# get the inverse of the matrix, NULL if not set
	getInverseMatrix <- function() invMatrix
	list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
# Check if the inverse of the given matrix has already been computed, if not compute it and return it, if already computed, skip the computation and return the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invMatrix <- x$getInverseMatrix()
	# check if the data is cached
	if (!is.null(invMatrix)) {
		message("getting cached data")
		# if so, print a nice message and return it
		return(invMatrix)
	}
	# otherwise get the matrix
	data <- x$get()
	# compute the inverse
	invMatrix <- solve(data, ...)
	x$setInverseMatrix(invMatrix)
	# and return it
	invMatrix
}
