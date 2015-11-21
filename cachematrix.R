## Put comments here that give an overall description of what your
## functions do

## this function initializes the list of functions to manage the matrix inversion
## input is a square matrix
## output is list of set, get, setInv and getInv functions

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## initializes the variables
	## uses cache operator
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(matrixInv) inv <<- matrixInv
	getInv <- function() inv
	## creates the list of functions
	list(set = set, get = get,
	setInv = setInv,
 	getInv = getInv)
}


## this function calculate the inverse of the matrix if it is not cached already
## Input is the list of functions returned by makeCacheMatrix function
## output is the inverse of the matrix

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	## checks if cache is not null
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## if cache is null, then calculates the inverse
	data <- x$get()
 	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}